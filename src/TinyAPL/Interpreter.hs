{-# LANGUAGE TupleSections, LambdaCase, FlexibleContexts #-}
module TinyAPL.Interpreter where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Functions as F
import qualified TinyAPL.Glyphs as G
import TinyAPL.Parser
import qualified TinyAPL.Primitives as P
import TinyAPL.Util

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Foldable (foldlM)
import Control.Monad
import Data.List
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Functor ( ($>) )
import Control.Monad.Except (MonadError)
import Data.Maybe (fromMaybe)

asWraps :: MonadError Error m => Error -> Array -> m Function
asWraps err arr = do
  if null $ arrayShape arr then asWrap err (headPromise $ arrayContents arr)
  else pure $ Function
    { functionMonad = Just $ \x -> F.onScalars1 (\w -> asScalar err w >>= asWrap err >>= (\f -> callMonad f x)) arr
    , functionDyad = Just $ \x y -> F.onScalars1 (\w -> asScalar err w >>= asWrap err >>= (\f -> callDyad f x y)) arr
    , functionRepr = [fst G.parens, G.unwrap] ++ show arr ++ [snd G.parens]
    , functionContext = Nothing }

data Value
  = VArray Array
  | VFunction Function
  | VAdverb Adverb
  | VConjunction Conjunction

valueCategory :: Value -> Category
valueCategory (VArray _) = CatArray
valueCategory (VFunction _) = CatFunction
valueCategory (VAdverb _) = CatAdverb
valueCategory (VConjunction _) = CatConjunction

instance Show Value where
  show (VArray arr)        = show arr
  show (VFunction fn)      = show fn
  show (VAdverb adv)       = show adv
  show (VConjunction conj) = show conj

unwrapArray :: Error -> Value -> St Array
unwrapArray _ (VArray val) = return val
unwrapArray e _            = throwError e

unwrapFunction :: Error -> Value -> St Function
unwrapFunction _ (VFunction val) = return val
unwrapFunction e _               = throwError e

unwrapAdverb :: Error -> Value -> St Adverb
unwrapAdverb _ (VAdverb val) = return val
unwrapAdverb e _             = throwError e

unwrapConjunction :: Error -> Value -> St Conjunction
unwrapConjunction _ (VConjunction val) = return val
unwrapConjunction e _                  = throwError e

callOnValue :: Adverb -> Value -> St Function
callOnValue adv (VArray x) = callOnArray adv x
callOnValue adv (VFunction x) = callOnFunction adv x
callOnValue _ _ = throwError $ DomainError "Invalid type to adverb call"

callOnValueAndValue :: Conjunction -> Value -> Value -> St Function
callOnValueAndValue conj (VArray x) (VArray y) = callOnArrayAndArray conj x y
callOnValueAndValue conj (VArray x) (VFunction y) = callOnArrayAndFunction conj x y
callOnValueAndValue conj (VFunction x) (VArray y) = callOnFunctionAndArray conj x y
callOnValueAndValue conj (VFunction x) (VFunction y) = callOnFunctionAndFunction conj x y
callOnValueAndValue _ _ _ = throwError $ DomainError "Invalid type to conjunction call"

makeValueAdverb :: (Value -> St Function) -> String -> Adverb
makeValueAdverb a s = Adverb
  { adverbOnArray = Just $ \x -> a (VArray x)
  , adverbOnFunction = Just $ \f -> a (VFunction f)
  , adverbRepr = s
  , adverbContext = Nothing }

makeValueConjunction :: (Value -> Value -> St Function) -> String -> Conjunction
makeValueConjunction a s = Conjunction
  { conjOnArrayArray = Just $ \x y -> a (VArray x) (VArray y)
  , conjOnArrayFunction = Just $ \x y -> a (VArray x) (VFunction y)
  , conjOnFunctionArray = Just $ \x y -> a (VFunction x) (VArray y)
  , conjOnFunctionFunction = Just $ \x y -> a (VFunction x) (VFunction y)
  , conjRepr = s
  , conjContext = Nothing }

scopeEntries :: Scope -> [(String, Value)]
scopeEntries sc = (second VArray <$> scopeArrays sc) ++ (second VFunction <$> scopeFunctions sc) ++ (second VAdverb <$> scopeAdverbs sc) ++ (second VConjunction <$> scopeConjunctions sc)

scopeShallowLookup :: String -> Scope -> Maybe Value
scopeShallowLookup name sc =
  VArray <$> scopeShallowLookupArray name sc
  <|> VFunction <$> scopeShallowLookupFunction name sc
  <|> VAdverb <$> scopeShallowLookupAdverb name sc
  <|> VConjunction <$> scopeShallowLookupConjunction name sc

scopeLookup :: String -> Scope -> St (Maybe Value)
scopeLookup name sc = do
  a <- scopeLookupArray name sc
  f <- scopeLookupFunction name sc
  adv <- scopeLookupAdverb name sc
  conj <- scopeLookupConjunction name sc
  pure $ (VArray <$> a) <|> (VFunction <$> f) <|> (VAdverb <$> adv) <|> (VConjunction <$> conj)

scopeUpdate :: String -> Value -> Scope -> Scope
scopeUpdate name (VArray val) sc       = scopeUpdateArray name val sc
scopeUpdate name (VFunction val) sc    = scopeUpdateFunction name val sc
scopeUpdate name (VAdverb val) sc      = scopeUpdateAdverb name val sc
scopeUpdate name (VConjunction val) sc = scopeUpdateConjunction name val sc

scopeModify :: String -> Value -> Scope -> St Scope
scopeModify name (VArray val) sc       = scopeModifyArray name val sc
scopeModify name (VFunction val) sc    = scopeModifyFunction name val sc
scopeModify name (VAdverb val) sc      = scopeModifyAdverb name val sc
scopeModify name (VConjunction val) sc = scopeModifyConjunction name val sc

inChildScope :: [(String, Value)] -> St a -> Context -> St a
inChildScope vals x parent = do
  ref <- createRef $ foldr (\(name, val) sc -> scopeUpdate name val sc) (Scope [] [] [] [] (Just $ contextScope parent)) vals
  runWithContext parent{ contextScope = ref } x

interpret :: Tree -> Context -> ResultIO (Value, Context)
interpret tree = runSt (eval tree)

forceTrees :: [Maybe Tree] -> [Tree]
forceTrees = fmap $ fromMaybe $ VectorBranch []

run :: String -> String -> Context -> ResultIO (Value, Context)
run file src scope = do
  trees <- except (parse file src)
  join $ foldlM (\last next -> interpret next . snd <$> last) (return (undefined, scope)) $ forceTrees trees

eval :: Tree -> St Value
eval (Leaf _ tok)                   = evalLeaf tok
eval (QualifiedBranch _ h ns)       = eval h >>= flip evalQualified ns
eval (MonadCallBranch l r)          = do
  r' <- eval r
  l' <- eval l
  evalMonadCall l' r'
eval (DyadCallBranch l r)           = do
  r' <- eval r
  l' <- eval l
  evalDyadCall l' r'
eval (AdverbCallBranch l r)         = do
  r' <- eval r
  l' <- eval l
  evalAdverbCall l' r'
eval (ConjunctionCallBranch l r)    = do
  r' <- eval r
  l' <- eval l
  evalConjunctionCall l' r'
eval (AssignBranch _ n t val)       = eval val >>= evalAssign n t
eval (QualifiedAssignBranch _ h ns val) = do
  rs <- eval val
  head <- eval h
  evalQualifiedAssign head ns rs
eval (VectorAssignBranch ns val)    = eval val >>= evalVectorAssign ns
eval (HighRankAssignBranch ns val)  = eval val >>= evalHighRankAssign ns
eval (DefinedBranch cat statements) = evalDefined statements cat
eval (GuardBranch _ _)              = throwError $ DomainError "Guards are not allowed outside of dfns"
eval (ExitBranch _)                 = throwError $ DomainError "Exits are not allowed outside of dfns"
eval (VectorBranch es)              = do
  entries <- mapM (eval >=> \case
    VArray x -> pure $ box x
    VFunction f -> pure $ Wrap f
    VAdverb a -> pure $ AdverbWrap a
    VConjunction c -> pure $ ConjunctionWrap c) $ reverse es
  return $ VArray $ vector $ reverse entries
eval (HighRankBranch es)            = do
  entries <- mapM (eval >=> unwrapArray (DomainError "Array notation entries must be arrays")) $ reverse es
  case entries of
    [] -> return $ VArray $ fromMajorCells []
    (e:es) ->
      if all ((== arrayShape e) . arrayShape) es
      then return $ VArray $ fromMajorCells $ reverse entries
      else throwError $ DomainError "High rank notation entries must be of the same shape"
eval (TrainBranch cat es)           = evalTrain cat es
eval (WrapBranch fn)                = eval fn >>= (\case
  VFunction fn -> pure $ VArray $ scalar $ Wrap fn
  VAdverb adv -> pure $ VArray $ scalar $ AdverbWrap adv
  VConjunction conj -> pure $ VArray $ scalar $ ConjunctionWrap conj
  _ -> throwError $ DomainError "Wrap notation: function or modifier required")
eval (UnwrapBranch cat fn)          = eval fn >>= evalUnwrap cat
eval (StructBranch es)              = evalStruct es

resolve :: Context -> [String] -> St Context
resolve ctx [] = pure ctx
resolve ctx (name:ns) = do
  sc <- readRef $ contextScope ctx
  arr <- scopeLookupArray name sc >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist"))
  asScalar (DomainError "Names of a qualifid identifier should be structs") arr
    >>= asStruct (DomainError "Names of a qualified identifier should be structs")
    >>= flip resolve ns

evalLeaf :: Token -> St Value
evalLeaf (TokenNumber [x] _)              = return $ VArray $ scalar $ Number x
evalLeaf (TokenNumber xs _)               = return $ VArray $ vector $ Number <$> xs
evalLeaf (TokenChar [x] _)                = return $ VArray $ scalar $ Character x
evalLeaf (TokenChar xs _)                 = return $ VArray $ vector $ Character <$> xs
evalLeaf (TokenString xs _)               = return $ VArray $ vector $ Character <$> xs
evalLeaf (TokenPrimArray n _)             =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive array " ++ [n]) $ VArray <$> lookup n P.arrays
evalLeaf (TokenPrimFunction n _)          =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive function " ++ [n]) $ VFunction <$> lookup n P.functions
evalLeaf (TokenPrimAdverb n _)            =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive adverb " ++ [n]) $ VAdverb <$> lookup n P.adverbs
evalLeaf (TokenPrimConjunction n _)       =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive conjunction " ++ [n]) $ VConjunction <$> lookup n P.conjunctions
evalLeaf (TokenArrayName name _)
  | name == [G.quad]                      = do
    out <- gets contextOut
    input <- gets contextIn
    out $ G.quad : ": "
    code <- input
    context <- get
    (res, context') <- lift $ run [G.quad] code context
    put $ context'
    return res
  | name == [G.quadQuote]                 = do
    input <- gets contextIn
    str <- input
    return $ VArray $ vector $ Character <$> str
  | isPrefixOf [G.quad] name              = do
    quads <- gets contextQuads
    let nilad = lookup name $ quadArrays quads
    case nilad of
      Just x -> VArray <$> getNilad x
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise                             =
    gets contextScope >>= readRef >>= scopeLookupArray name >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VArray)
-- evalLeaf (TokenArrayName names val)       = do
--   let (q, l) = fromJust $ unsnoc names
--   ctx <- resolve q
--   runWithContext ctx $ evalLeaf $ TokenArrayName [l] val
evalLeaf (TokenFunctionName name _)
  | isPrefixOf [G.quad] name              = do
    quads <- gets contextQuads
    let fn = lookup name $ quadFunctions quads
    case fn of
      Just x -> return $ VFunction x
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise                             =
    gets contextScope >>= readRef >>= scopeLookupFunction name >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VFunction)
-- evalLeaf (TokenFunctionName names val)    = do
--   let (q, l) = fromJust $ unsnoc names
--   ctx <- resolve q
--   runWithContext ctx $ evalLeaf $ TokenFunctionName [l] val
evalLeaf (TokenAdverbName name _)
  | isPrefixOf [G.quad] name              = do
    quads <- gets contextQuads
    let adv = lookup name $ quadAdverbs quads
    case adv of
      Just x -> return $ VAdverb x
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise                             =
    gets contextScope >>= readRef >>= scopeLookupAdverb name >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VAdverb)
-- evalLeaf (TokenAdverbName names val)      = do
--   let (q, l) = fromJust $ unsnoc names
--   ctx <- resolve q
--   runWithContext ctx $ evalLeaf $ TokenAdverbName [l] val
evalLeaf (TokenConjunctionName name _)
  | isPrefixOf [G.quad] name              = do
    quads <- gets contextQuads
    let conj = lookup name $ quadConjunctions quads
    case conj of
      Just x -> return $ VConjunction x
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise                             =
    gets contextScope >>= readRef >>= scopeLookupConjunction name >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VConjunction)
-- evalLeaf (TokenConjunctionName names val) = do
--   let (q, l) = fromJust $ unsnoc names
--   ctx <- resolve q
--   runWithContext ctx $ evalLeaf $ TokenConjunctionName [l] val
evalLeaf _                             = throwError $ DomainError "Invalid leaf type in evaluation"

evalQualified :: Value -> NonEmpty String -> St Value
evalQualified head ns = do
  let err = DomainError "Qualified name head should be a scalar struct"
  let (q, l) = unsnocNE ns
  headCtx <- unwrapArray err head >>= asScalar err >>= asStruct err
  ctx <- resolve headCtx q
  scope <- readRef $ contextScope ctx
  lift $ except $ maybeToEither (SyntaxError $ "Qualified variable " ++ l ++ " does not exist") $ scopeShallowLookup l scope

evalMonadCall :: Value -> Value -> St Value
evalMonadCall (VFunction fn) (VArray arr) = VArray <$> callMonad fn arr
evalMonadCall _ _                         = throwError $ DomainError "Invalid arguments to monad call evaluation"

evalDyadCall :: Value -> Value -> St Value
evalDyadCall (VArray arr) (VFunction f) =
  return $ VFunction $ Function { functionMonad = Just $ callDyad f arr, functionDyad = Nothing, functionRepr = "(" ++ show arr ++ show f ++ ")", functionContext = functionContext f }
evalDyadCall _ _                        = throwError $ DomainError "Invalid arguments to dyad call evaluation"

evalAdverbCall :: Value -> Value -> St Value
evalAdverbCall (VArray l) (VAdverb adv)    = VFunction <$> callOnArray adv l
evalAdverbCall (VFunction l) (VAdverb adv) = VFunction <$> callOnFunction adv l
evalAdverbCall _ _                         = throwError $ DomainError "Invalid arguments to adverb call evaluation"

evalConjunctionCall :: Value -> Value -> St Value
evalConjunctionCall (VConjunction conj) (VArray r)    =
  return $ VAdverb $ Adverb { adverbOnArray = Just (\x -> callOnArrayAndArray conj x r), adverbOnFunction = Just (\x -> callOnFunctionAndArray conj x r), adverbRepr = "(" ++ show conj ++ show r ++ ")", adverbContext = conjContext conj }
evalConjunctionCall (VConjunction conj) (VFunction r) =
  return $ VAdverb $ Adverb { adverbOnArray = Just (\x -> callOnArrayAndFunction conj x r), adverbOnFunction = Just (\x -> callOnFunctionAndFunction conj x r), adverbRepr = "(" ++ show conj ++ show r ++ ")", adverbContext = conjContext conj }
evalConjunctionCall _ _                               = throwError $ DomainError "Invalid arguments to conjunction call evaluation"

evalAssign :: String -> AssignType -> Value -> St Value
evalAssign name ty val
  | name == [G.quad] = if ty == AssignNormal then do
    arr <- unwrapArray (DomainError "Cannot print non-array") val
    out <- gets contextOut
    out $ show arr ++ "\n"
    return val 
    else throwError $ DomainError "Can only assign normally to quads"
  | name == [G.quadQuote] = if ty == AssignNormal then do
    arr <- unwrapArray (DomainError "Cannot print non-array") val
    err <- gets contextErr
    err $ show arr
    return val
    else throwError $ DomainError "Can only assign normally to quads"
  | isPrefixOf [G.quad] name = if ty == AssignNormal then do
    arr <- unwrapArray (DomainError "Cannot set quad name to non-array") val
    quads <- gets contextQuads
    let nilad = lookup name $ quadArrays quads
    case nilad of
      Just x -> setNilad x arr $> val
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
    else throwError $ DomainError "Can only assign normally to quads"
  | ty == AssignNormal = do
    gets contextScope >>= flip modifyRef (scopeUpdate name val)
    return val
  | ty == AssignModify = do
    sc <- gets contextScope
    readRef sc >>= scopeModify name val >>= writeRef sc
    return val
  | otherwise = throwError unreachable

evalQualifiedAssign :: Value -> NonEmpty String -> Value -> St Value
evalQualifiedAssign head ns val = do
  let err = DomainError "Qualified name head should be a scalar struct"
  let (q, l) = unsnocNE ns
  headCtx <- unwrapArray err head >>= asScalar err >>= asStruct err
  ctx <- resolve headCtx q
  scope <- readRef $ contextScope ctx
  scopeLookup l scope >>= lift . except . maybeToEither (SyntaxError $ "Qualified variable " ++ l ++ " does not exist")
  runWithContext ctx $ evalAssign l AssignNormal val

evalVectorAssign :: [String] -> Value -> St Value
evalVectorAssign ns val =
  if any (\(n:_) -> n == G.quad || n == G.quadQuote) ns then throwError $ DomainError "Vector assignment: cannot assign to quad names"
  else do
    es <- fmap fromScalar <$> (unwrapArray (DomainError "Vector assign: not a vector") val >>= asVector (DomainError "Vector assignment: not a vector"))
    if length ns /= length es then throwError $ DomainError "Vector assignment: wrong number of names"
    else do
      scope <- gets contextScope
      modifyRef scope $ (\sc -> foldr (\(n, e) sc' -> scopeUpdateArray n e sc') sc $ zip ns es)
      return val

evalHighRankAssign :: [String] -> Value -> St Value
evalHighRankAssign ns val =
  if any (\(n:_) -> n == G.quad || n == G.quadQuote) ns then throwError $ DomainError "High rank assignment: cannot assign to quad names"
  else do
    es <- majorCells <$> unwrapArray (DomainError "High rank assign: not an array") val
    if length ns /= length es then throwError $ DomainError "High rank assignment: wrong number of names"
    else do
      scope <- gets contextScope
      modifyRef scope $ (\sc -> foldr (\(n, e) sc' -> scopeUpdateArray n e sc') sc $ zip ns es)
      return val

evalDefined :: NonEmpty Tree -> Category -> St Value
evalDefined statements cat = let
  ev :: Tree -> St (Value, Bool)
  ev (GuardBranch check result) = do
    c <- eval check >>= unwrapArray (DomainError "Guard check not array") >>= lift . except . asScalar (DomainError "Guard check not scalar") >>= lift . except . asBool (DomainError "Guard check not boolean")
    if c then ev result
    else return (VArray $ Array [0, 0] [], False)
  ev (ExitBranch result) = (, True) <$> eval result
  ev other = (, False) <$> eval other

  runDefined :: NonEmpty Tree -> St Value
  runDefined (x :| xs) = case NE.nonEmpty xs of
    Nothing -> fst <$> ev x
    Just xs -> do
      (v, r) <- ev x
      if r then return v else runDefined xs

  run xs sc = inChildScope xs (runDefined statements) sc >>= unwrapArray (DomainError "Dfn must return an array")
  in do
    sc <- get
    case cat of
      CatFunction -> let
        dfn = VFunction (Function
          { functionRepr = "{...}"
          , functionContext = Just $ sc
          , functionMonad = Just $ \x -> run [([G.omega], VArray x), ([G.del], dfn)] sc
          , functionDyad = Just $ \x y -> run [([G.alpha], VArray x), ([G.omega], VArray y), ([G.del], dfn)] sc } )
        in return dfn
      CatAdverb -> let
        dadv = VAdverb (Adverb
          { adverbRepr = "_{...}"
          , adverbContext = Just $ sc
          , adverbOnArray = Just $ \a -> let
            dfn = (Function
              { functionRepr = "(" ++ show a ++ ")_{...}"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alpha, G.alpha], VArray a)
                , ([G.omega], VArray x)
                , ([G.underscore, G.del], dadv)
                , ([G.del], VFunction dfn) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alpha, G.alpha], VArray a)
                , ([G.alpha], VArray x)
                , ([G.omega], VArray y)
                , ([G.underscore, G.del], dadv)
                , ([G.del], VFunction dfn) ] sc } )
            in return dfn
          , adverbOnFunction = Just $ \a -> let
            dfn = (Function
              { functionRepr = "(" ++ show a ++ ")_{...}"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alphaBar, G.alphaBar], VFunction a)
                , ([G.omega], VArray x)
                , ([G.underscore, G.del], dadv)
                , ([G.del], VFunction dfn) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alphaBar, G.alphaBar], VFunction a)
                , ([G.alpha], VArray x)
                , ([G.omega], VArray y)
                , ([G.underscore, G.del], dadv)
                , ([G.del], VFunction dfn) ] sc } )
            in return dfn } )
        in return dadv
      CatConjunction -> let
        dconj = VConjunction (Conjunction
          { conjRepr = "_{...}_"
          , conjContext = Just $ sc
          , conjOnArrayArray = Just $ \a b -> let
            dfn = (Function
              { functionRepr = "(" ++ show a ++ ")_{...}_(" ++ show b ++ ")"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alpha, G.alpha], VArray a)
                , ([G.omega, G.omega], VArray b)
                , ([G.omega], VArray x)
                , ([G.underscore, G.del, G.underscore], dconj)
                , ([G.del], VFunction dfn) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alpha, G.alpha], VArray a)
                , ([G.omega, G.omega], VArray b)
                , ([G.alpha], VArray x)
                , ([G.omega], VArray y)
                , ([G.underscore, G.del, G.underscore], dconj)
                , ([G.del], VFunction dfn) ] sc } )
            in return dfn
          , conjOnArrayFunction = Just $ \a b -> let
            dfn = (Function
              { functionRepr = "(" ++ show a ++ ")_{...}_(" ++ show b ++ ")"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alpha, G.alpha], VArray a)
                , ([G.omegaBar, G.omegaBar], VFunction b)
                , ([G.omega], VArray x)
                , ([G.underscore, G.del, G.underscore], dconj)
                , ([G.del], VFunction dfn) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alpha, G.alpha], VArray a)
                , ([G.omegaBar, G.omegaBar], VFunction b)
                , ([G.alpha], VArray x)
                , ([G.omega], VArray y)
                , ([G.underscore, G.del, G.underscore], dconj)
                , ([G.del], VFunction dfn) ] sc } )
            in return dfn
          , conjOnFunctionArray = Just $ \a b -> let
            dfn = (Function
              { functionRepr = "(" ++ show a ++ ")_{...}_(" ++ show b ++ ")"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alphaBar, G.alphaBar], VFunction a)
                , ([G.omega, G.omega], VArray b)
                , ([G.omega], VArray x)
                , ([G.underscore, G.del, G.underscore], dconj)
                , ([G.del], VFunction dfn) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alphaBar, G.alphaBar], VFunction a)
                , ([G.omega, G.omega], VArray b)
                , ([G.alpha], VArray x)
                , ([G.omega], VArray y)
                , ([G.underscore, G.del, G.underscore], dconj)
                , ([G.del], VFunction dfn) ] sc } )
            in return dfn
          , conjOnFunctionFunction = Just $ \a b -> let
            dfn = (Function
              { functionRepr = "(" ++ show a ++ ")_{...}_(" ++ show b ++ ")"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alphaBar, G.alphaBar], VFunction a)
                , ([G.omegaBar, G.omegaBar], VFunction b)
                , ([G.omega], VArray x)
                , ([G.underscore, G.del, G.underscore], dconj)
                , ([G.del], VFunction dfn) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alphaBar, G.alphaBar], VFunction a)
                , ([G.omegaBar, G.omegaBar], VFunction b)
                , ([G.alpha], VArray x)
                , ([G.omega], VArray y)
                , ([G.underscore, G.del, G.underscore], dconj)
                , ([G.del], VFunction dfn) ] sc } )
            in return dfn } )
        in return dconj
      cat -> throwError $ DomainError $ "Defined of type " ++ show cat ++ "?"

evalTrain :: Category -> [Maybe Tree] -> St Value
evalTrain cat es = let
  atop :: Function -> Function -> Function
  atop f g = Function { functionMonad = Just $ F.compose (callMonad f) (callMonad g), functionDyad = Just $ F.atop (callMonad f) (callDyad g), functionRepr = "", functionContext = Nothing }

  fork :: Function -> Function -> Function -> Function
  fork f g h = Function { functionMonad = Just $ F.fork1 (callMonad f) (callDyad g) (callMonad h), functionDyad = Just $ F.fork2 (callDyad g) (callDyad f) (callDyad h), functionRepr = "", functionContext = Nothing }

  bindLeft :: Function -> Array -> Function
  bindLeft f x = Function { functionMonad = Just $ \y -> callDyad f x y, functionDyad = Nothing, functionRepr = "", functionContext = Nothing }

  bindRight :: Function -> Array -> Function
  bindRight f y = Function { functionMonad = Just $ \x -> callDyad f x y, functionDyad = Nothing, functionRepr = "", functionContext = Nothing }

  train1 :: Value -> St Value
  train1 (VArray x) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 x, functionDyad = Just $ F.constant2 x, functionRepr = "", functionContext = Nothing }
  train1 o = pure $ o

  train2 :: Value -> Value -> St Value
  train2 (VArray x) (VArray _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 x, functionDyad = Just $ F.constant2 x, functionRepr = "", functionContext = Nothing }
  train2 (VArray x) (VFunction g) = pure $ VFunction $ Function { functionMonad = Just $ \y -> callDyad g x y, functionDyad = Nothing, functionRepr = "", functionContext = Nothing}
  train2 (VFunction f) (VArray y) = pure $ VFunction $ Function { functionMonad = Just $ \x -> callDyad f x y, functionDyad = Nothing, functionRepr = "", functionContext = Nothing }
  train2 (VFunction f) (VFunction g) = pure $ VFunction $ atop f g
  train2 (VArray x) (VAdverb a) = VFunction <$> callOnArray a x 
  train2 (VFunction f) (VAdverb a) = VFunction <$> callOnFunction a f
  train2 (VAdverb a) (VFunction g) = pure $ VAdverb $ makeValueAdverb (\u -> (`atop` g) <$> callOnValue a u) ""
  train2 (VAdverb a) (VAdverb b) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValue a u >>= callOnFunction b) ""
  train2 (VAdverb a) (VConjunction c) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValue a u >>= (\r -> callOnValueAndValue c (VFunction r) u)) ""
  train2 x@(VArray _) (VConjunction c) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValueAndValue c x u) ""
  train2 f@(VFunction _) (VConjunction c) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValueAndValue c f u) ""
  train2 (VConjunction c) y@(VArray _) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValueAndValue c u y) ""
  train2 (VConjunction c) g@(VFunction _) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValueAndValue c u g) ""
  train2 (VConjunction c) (VAdverb a) = pure $ VConjunction $ makeValueConjunction (\u v -> callOnValueAndValue c u v >>= callOnFunction a) ""
  train2 (VConjunction c) (VConjunction d) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    a <- callOnValueAndValue d u v
    b <- callOnValueAndValue c u v
    pure $ atop a b) ""
  train2 x y = throwError $ DomainError $ "2-train with " ++ show x ++ " and " ++ show y ++ "?"

  train3 :: Value -> Value -> Value -> St Value
  train3 (VArray _) (VArray y) (VArray _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "", functionContext = Nothing }
  train3 (VArray _) (VArray y) (VFunction _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "", functionContext = Nothing }
  train3 (VFunction _) (VArray y) (VArray _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "", functionContext = Nothing }
  train3 (VFunction _) (VArray y) (VFunction _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "", functionContext = Nothing }
  train3 (VFunction f) (VFunction g) (VFunction h) = pure $ VFunction $ fork f g h
  train3 (VArray x) (VFunction g) (VFunction h) = pure $ VFunction $ atop (bindLeft g x) h
  train3 (VFunction f) (VFunction g) (VArray z) = pure $ VFunction $ atop (bindRight g z) f
  train3 (VArray x) (VFunction g) (VArray z) = do
    r <- callDyad g x z
    pure $ VFunction $ Function { functionMonad = Just $ F.constant1 r, functionDyad = Just $ F.constant2 r, functionRepr = "", functionContext = Nothing }
  train3 (VArray x) (VConjunction c) (VArray z) = VFunction <$>callOnArrayAndArray c x z
  train3 (VArray x) (VConjunction c) (VFunction h) = VFunction <$> callOnArrayAndFunction c x h
  train3 (VFunction f) (VConjunction c) (VArray z) = VFunction <$> callOnFunctionAndArray c f z
  train3 (VFunction f) (VConjunction c) (VFunction h) = VFunction <$> callOnFunctionAndFunction c f h
  train3 (VAdverb a) (VFunction g) (VFunction h) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    pure $ fork r g h) ""
  train3 (VAdverb a) (VAdverb b) (VAdverb c) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    s <- callOnFunction b r
    callOnFunction c s) ""
  train3 (VArray x) (VConjunction c) (VAdverb a) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    callOnArrayAndFunction c x r) ""
  train3 (VFunction f) (VConjunction c) (VAdverb a) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    callOnFunctionAndFunction c f r) ""
  train3 (VAdverb a) (VConjunction c) (VArray z) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    callOnFunctionAndArray c r z) ""
  train3 (VAdverb a) (VConjunction c) (VFunction h) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    callOnFunctionAndFunction c r h) ""
  train3 (VFunction f) (VFunction g) (VConjunction c) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue c u v
    pure $ fork f g r) ""
  train3 (VArray x) (VFunction g) (VConjunction c) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue c u v
    pure $ atop (bindLeft g x) r) ""
  train3 (VConjunction c) (VFunction g) (VFunction h) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue c u v
    pure $ fork r g h) ""
  train3 (VConjunction c) (VFunction g) (VConjunction d) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue d u v
    s <- callOnValueAndValue c u v
    pure $ fork s g r) ""
  train3 (VAdverb a) (VAdverb b) (VFunction h) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValue b v
    s <- callOnValue a u
    pure $ fork s r h) ""
  train3 (VConjunction c) (VAdverb a) (VAdverb b) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue c u v
    s <- callOnFunction a r
    callOnFunction b s) ""
  train3 (VArray x) (VConjunction c) (VConjunction d) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue d u v
    callOnArrayAndFunction c x r) ""
  train3 (VFunction f) (VConjunction c) (VConjunction d) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue d u v
    callOnFunctionAndFunction c f r) ""
  train3 (VAdverb a) (VConjunction c) (VAdverb b) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValue b v
    s <- callOnValue a u
    callOnFunctionAndFunction c s r) ""
  train3 (VAdverb a) (VConjunction c) (VConjunction d) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue d u v
    s <- callOnValue a u
    callOnFunctionAndFunction c s r) ""
  train3 (VConjunction c) (VConjunction d) (VArray z) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue c u v
    callOnFunctionAndArray d r z) ""
  train3 (VConjunction c) (VConjunction d) (VFunction h) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue c u v
    callOnFunctionAndFunction d r h) ""
  train3 (VConjunction c) (VConjunction d) (VAdverb a) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValue a v
    s <- callOnValueAndValue c u v
    callOnFunctionAndFunction d s r) ""
  train3 (VConjunction c) (VConjunction d) (VConjunction e) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue e u v
    s <- callOnValueAndValue c u v
    callOnFunctionAndFunction d s r) ""
  train3 x y z = throwError $ DomainError $ "3-train with " ++ show x ++ ", " ++ show y ++ " and " ++ show z ++ "?"

  train :: [Maybe Value] -> St Value
  train [] = throwError $ DomainError "Empty train"
  train [Nothing] = throwError $ DomainError "Empty train"
  train [Just x] = train1 x
  train [Just y, Just x] = train2 x y
  train [_, _] = throwError $ SyntaxError "2-train cannot contain empty entries"
  train (Just z : Just y : Just x : rs) = train3 x y z >>= train . (: rs) . Just
  train (Just z : Just y : Nothing : rs) = train2 y z >>= train . (: rs) . Just
  train _ = throwError $ SyntaxError "3-train can only contain empty entries as the first tine"

  showTine :: Maybe Value -> String
  showTine Nothing = ""
  showTine (Just x) = show x

  withTrainRepr :: [Maybe Value] -> Value -> St Value
  withTrainRepr _ (VArray _) = throwError $ DomainError "Array train?"
  withTrainRepr us (VFunction f) = pure $ VFunction $ f{ functionRepr = [fst G.train] ++ intercalate [' ', G.separator, ' '] (showTine <$> us) ++ [snd G.train] }
  withTrainRepr us (VAdverb a) = pure $ VAdverb $ a{ adverbRepr = [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] (showTine <$> us) ++ [snd G.train] }
  withTrainRepr us (VConjunction c) = pure $ VConjunction $ c{ conjRepr = [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] (showTine <$> us) ++ [snd G.train, G.underscore] }
  in do
    us <- mapM (\case
      Nothing -> pure Nothing
      Just x -> Just <$> eval x) $ reverse es
    t <- train us
    r <- withTrainRepr us t
    case (cat, r) of
      (CatArray, _) -> throwError $ DomainError "Array train?"
      (CatFunction, r@(VFunction _)) -> pure r
      (CatAdverb, r@(VAdverb _)) -> pure r
      (CatConjunction, r@(VConjunction _)) -> pure r
      (exp, g) -> throwError $ DomainError $ "Expected train of category " ++ show exp ++ ", got a " ++ show (valueCategory g)

evalStruct :: [Tree] -> St Value
evalStruct statements = do
  ctx <- get
  newScope <- createRef $ Scope [] [] [] [] (Just $ contextScope ctx)
  let newContext = ctx{ contextScope = newScope }
  mapM_ (runWithContext newContext . eval) statements
  pure $ VArray $ scalar $ Struct newContext

evalUnwrap :: Category -> Value -> St Value
evalUnwrap CatFunction v = do
  let err = DomainError "Unwrap notation: array of wraps required"
  VFunction <$> (unwrapArray err v >>= asWraps err)
evalUnwrap CatAdverb v = do
  let err = DomainError "Unwrap adverb notation: scalar array wrap required"
  arr <- unwrapArray err v
  if null $ arrayShape arr then VAdverb <$> asAdverbWrap err (headPromise $ arrayContents arr)
  else throwError err
evalUnwrap CatConjunction v = do
  let err = DomainError "Unwrap conjunction notation: scalar array wrap required"
  arr <- unwrapArray err v
  if null $ arrayShape arr then VConjunction <$> asConjunctionWrap err (headPromise $ arrayContents arr)
  else throwError err
evalUnwrap _ _ = throwError unreachable
