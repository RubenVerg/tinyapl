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
import Control.Monad
import Data.Bifunctor
import Data.List
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Functor ( ($>) )
import Control.Monad.Except (MonadError)
import Data.Maybe (fromMaybe)
import Data.Foldable (foldrM)

asWraps :: MonadError Error m => Error -> Noun -> m Function
asWraps err arr = do
  if null $ arrayShape arr then asWrap err (headPromise $ arrayContents arr)
  else pure $ UnwrapArrayFunction
    { functionMonad = Just $ \x -> F.onScalars1 (\w -> asScalar err w >>= asWrap err >>= (\f -> callMonad f x)) arr
    , functionDyad = Just $ \x y -> F.onScalars1 (\w -> asScalar err w >>= asWrap err >>= (\f -> callDyad f x y)) arr
    , functionContext = Nothing
    , unwrapFunctionArray = arr }

valueCategory :: Value -> Category
valueCategory (VNoun _) = CatArray
valueCategory (VFunction _) = CatFunction
valueCategory (VAdverb _) = CatAdverb
valueCategory (VConjunction _) = CatConjunction

scopeEntries :: Scope -> [(String, (VariableType, Value))]
scopeEntries sc = (second (second VNoun) <$> scopeNouns sc) ++ (second (second VFunction) <$> scopeFunctions sc) ++ (second (second VAdverb) <$> scopeAdverbs sc) ++ (second (second VConjunction) <$> scopeConjunctions sc)

scopeShallowLookup :: Bool -> String -> Scope -> Maybe Value
scopeShallowLookup private name sc =
  VNoun <$> scopeShallowLookupNoun private name sc
  <|> VFunction <$> scopeShallowLookupFunction private name sc
  <|> VAdverb <$> scopeShallowLookupAdverb private name sc
  <|> VConjunction <$> scopeShallowLookupConjunction private name sc

scopeLookup :: Bool -> String -> Scope -> St (Maybe Value)
scopeLookup private name sc = do
  a <- scopeLookupNoun private name sc
  f <- scopeLookupFunction private name sc
  adv <- scopeLookupAdverb private name sc
  conj <- scopeLookupConjunction private name sc
  pure $ (VNoun <$> a) <|> (VFunction <$> f) <|> (VAdverb <$> adv) <|> (VConjunction <$> conj)

scopeUpdate :: Bool -> String -> VariableType -> Value -> Scope -> St Scope
scopeUpdate private name ty (VNoun val) sc        = scopeUpdateNoun private name ty val sc
scopeUpdate private name ty (VFunction val) sc    = scopeUpdateFunction private name ty val sc
scopeUpdate private name ty (VAdverb val) sc      = scopeUpdateAdverb private name ty val sc
scopeUpdate private name ty (VConjunction val) sc = scopeUpdateConjunction private name ty val sc

scopeModify :: Bool -> String -> Value -> Scope -> St Scope
scopeModify private name (VNoun val) sc        = scopeModifyNoun private name val sc
scopeModify private name (VFunction val) sc    = scopeModifyFunction private name val sc
scopeModify private name (VAdverb val) sc      = scopeModifyAdverb private name val sc
scopeModify private name (VConjunction val) sc = scopeModifyConjunction private name val sc

scopeShallowModify :: Bool -> String -> Value -> Scope -> St Scope
scopeShallowModify private name (VNoun val) sc        = scopeShallowModifyNoun private name val sc
scopeShallowModify private name (VFunction val) sc    = scopeShallowModifyFunction private name val sc
scopeShallowModify private name (VAdverb val) sc      = scopeShallowModifyAdverb private name val sc
scopeShallowModify private name (VConjunction val) sc = scopeShallowModifyConjunction private name val sc

inChildScope :: [(String, (VariableType, Value))] -> St a -> Context -> St a
inChildScope vals x parent = do
  ref <- foldrM (\(name, (t, val)) sc -> scopeUpdate True name t val sc) (Scope [] [] [] [] (Just $ contextScope parent)) vals >>= createRef
  runWithContext parent{ contextScope = ref } x

interpret :: Tree -> Context -> ResultIO (Value, Context)
interpret tree = runSt (eval tree)

forceTrees :: [Maybe Tree] -> [Tree]
forceTrees = fmap $ fromMaybe $ VectorBranch []

run :: FilePath -> String -> Context -> ResultIO (Value, Context)
run file src = runSt (run' file src)

run' :: FilePath -> String -> St Value
run' file src = do
  trees <- lift $ except (parse file src)
  last <$> mapM eval (forceTrees trees)

eval :: Tree -> St Value
eval (Leaf _ tok) = evalLeaf tok
eval (QualifiedBranch _ h ns) = eval h >>= flip evalQualified ns
eval (MonadCallBranch l r) = do
  r' <- eval r
  l' <- eval l
  evalMonadCall l' r'
eval (DyadCallBranch l r) = do
  r' <- eval r
  l' <- eval l
  evalDyadCall l' r'
eval (AdverbCallBranch l r) = do
  r' <- eval r
  l' <- eval l
  evalAdverbCall l' r'
eval (ConjunctionCallBranch l r) = do
  r' <- eval r
  l' <- eval l
  evalConjunctionCall l' r'
eval (AssignBranch _ n t val) = eval val >>= evalAssign False True n t
eval (QualifiedAssignBranch _ h ns c val) = do
  rs <- eval val
  head <- eval h
  evalQualifiedAssign head ns c rs
eval (VectorAssignBranch ns c val) = eval val >>= evalVectorAssign ns c
eval (HighRankAssignBranch ns c val) = eval val >>= evalHighRankAssign ns c
eval (StructAssignBranch ns c val) = eval val >>= evalStructAssign ns c
eval (DefinedBranch cat statements) = evalDefined statements cat
eval (GuardBranch _ _) = throwError $ DomainError "Guards are not allowed outside of dfns"
eval (ExitBranch _) = throwError $ DomainError "Exits are not allowed outside of dfns"
eval (VectorBranch es) = do
  entries <- mapM (eval >=> \case
    VNoun x -> pure $ box x
    VFunction f -> pure $ Wrap f
    VAdverb a -> pure $ AdverbWrap a
    VConjunction c -> pure $ ConjunctionWrap c) $ reverse es
  return $ VNoun $ vector $ reverse entries
eval (HighRankBranch es) = do
  entries <- mapM (eval >=> unwrapNoun (DomainError "Array notation entries must be arrays")) $ reverse es
  case entries of
    [] -> return $ VNoun $ fromMajorCells []
    (e:es) ->
      if all ((== arrayShape e) . arrayShape) es
      then return $ VNoun $ fromMajorCells $ reverse entries
      else throwError $ DomainError "High rank notation entries must be of the same shape"
eval (DictionaryBranch es) = do
  let toScalar (VNoun x) = box x
      toScalar (VFunction f) = Wrap f
      toScalar (VAdverb a) = AdverbWrap a
      toScalar (VConjunction c) = ConjunctionWrap c
  entries <- mapM (\(k, v) -> liftA2 (,) (toScalar <$> eval k) (toScalar <$> eval v)) es
  return $ VNoun $ dictionary entries
eval (TrainBranch cat es) = evalTrain cat es
eval (WrapBranch fn) = eval fn >>= (\case
  VFunction fn -> pure $ VNoun $ scalar $ Wrap fn
  VAdverb adv -> pure $ VNoun $ scalar $ AdverbWrap adv
  VConjunction conj -> pure $ VNoun $ scalar $ ConjunctionWrap conj
  _ -> throwError $ DomainError "Wrap notation: function or modifier required")
eval (UnwrapBranch cat fn) = eval fn >>= evalUnwrap cat
eval (StructBranch es) = evalStruct es
eval (TernaryBranch cond true false) = do
  let err = DomainError "Ternary condition must be a scalar boolean"
  c <- eval cond >>= unwrapNoun err >>= asScalar err >>= asBool err
  if c then eval true else eval false

resolve :: Context -> [String] -> St Context
resolve ctx [] = pure ctx
resolve ctx (name:ns) = do
  sc <- readRef $ contextScope ctx
  arr <- scopeLookupNoun False name sc >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist"))
  asScalar (DomainError "Names of a qualifid identifier should be structs") arr
    >>= asStruct (DomainError "Names of a qualified identifier should be structs")
    >>= flip resolve ns

evalLeaf :: Token -> St Value
evalLeaf (TokenNumber x _  )              = return $ VNoun $ scalar $ Number x
evalLeaf (TokenChar [x] _)                = return $ VNoun $ scalar $ Character x
evalLeaf (TokenChar xs _)                 = return $ VNoun $ vector $ Character <$> xs
evalLeaf (TokenString xs _)               = return $ VNoun $ vector $ Character <$> xs
evalLeaf (TokenPrimArray n _)             =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive array " ++ [n]) $ VNoun <$> lookup n P.arrays
evalLeaf (TokenPrimFunction n _)          =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive function " ++ [n]) $ VFunction <$> lookup n P.functions
evalLeaf (TokenPrimAdverb n _)            =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive adverb " ++ [n]) $ VAdverb <$> lookup n P.adverbs
evalLeaf (TokenPrimConjunction n _)       =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive conjunction " ++ [n]) $ VConjunction <$> lookup n P.conjunctions
evalLeaf (TokenArrayName name _)
  | name == [G.quad]                      = do
    err <- gets contextErr
    input <- gets contextIn
    err $ G.quad : ": "
    code <- input
    context <- get
    (res, context') <- lift $ run [G.quad] code context
    put $ context'
    return res
  | name == [G.quadQuote]                 = do
    input <- gets contextIn
    str <- input
    return $ VNoun $ vector $ Character <$> str
  | isPrefixOf [G.quad] name              = do
    quads <- gets contextQuads
    let nilad = lookup name $ quadArrays quads
    case nilad of
      Just x -> VNoun <$> getNilad x
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise                             =
    gets contextScope >>= readRef >>= scopeLookupNoun True name >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VNoun)
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
    gets contextScope >>= readRef >>= scopeLookupFunction True name >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VFunction)
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
    gets contextScope >>= readRef >>= scopeLookupAdverb True name >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VAdverb)
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
    gets contextScope >>= readRef >>= scopeLookupConjunction True name >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VConjunction)
-- evalLeaf (TokenConjunctionName names val) = do
--   let (q, l) = fromJust $ unsnoc names
--   ctx <- resolve q
--   runWithContext ctx $ evalLeaf $ TokenConjunctionName [l] val
evalLeaf _                             = throwError $ DomainError "Invalid leaf type in evaluation"

evalQualified :: Value -> NonEmpty String -> St Value
evalQualified head ns = do
  let err = DomainError "Qualified name head should be a scalar struct"
  let (q, l) = unsnocNE ns
  headCtx <- unwrapNoun err head >>= asScalar err >>= asStruct err
  ctx <- resolve headCtx q
  scope <- readRef $ contextScope ctx
  lift $ except $ maybeToEither (SyntaxError $ "Qualified variable " ++ l ++ " does not exist") $ scopeShallowLookup False l scope

evalMonadCall :: Value -> Value -> St Value
evalMonadCall (VFunction fn) (VNoun arr) = VNoun <$> callMonad fn arr
evalMonadCall _ _                        = throwError $ DomainError "Invalid arguments to monad call evaluation"

evalDyadCall :: Value -> Value -> St Value
evalDyadCall (VNoun arr) (VFunction f) =
  return $ VFunction $ PartialFunction { functionMonad = Just $ callDyad f arr, functionDyad = Nothing, functionContext = functionContext f, partialFunctionFunction = f, partialFunctionLeft = arr }
evalDyadCall _ _                       = throwError $ DomainError "Invalid arguments to dyad call evaluation"

evalAdverbCall :: Value -> Value -> St Value
evalAdverbCall (VNoun l) (VAdverb adv)     = VFunction <$> callOnNoun adv l
evalAdverbCall (VFunction l) (VAdverb adv) = VFunction <$> callOnFunction adv l
evalAdverbCall _ _                         = throwError $ DomainError "Invalid arguments to adverb call evaluation"

evalConjunctionCall :: Value -> Value -> St Value
evalConjunctionCall (VConjunction conj) (VNoun r)     =
  return $ VAdverb $ PartialAdverb { adverbOnNoun = Just (\x -> callOnNounAndNoun conj x r), adverbOnFunction = Just (\x -> callOnFunctionAndNoun conj x r), adverbContext = conjContext conj, partialAdverbConjunction = conj, partialAdverbRight = VNoun r }
evalConjunctionCall (VConjunction conj) (VFunction r) =
  return $ VAdverb $ PartialAdverb { adverbOnNoun = Just (\x -> callOnNounAndFunction conj x r), adverbOnFunction = Just (\x -> callOnFunctionAndFunction conj x r), adverbContext = conjContext conj, partialAdverbConjunction = conj, partialAdverbRight = VFunction r }
evalConjunctionCall _ _                               = throwError $ DomainError "Invalid arguments to conjunction call evaluation"

evalAssign :: Bool -> Bool -> String -> AssignType -> Value -> St Value
evalAssign shallow private name ty val
  | name == [G.quad] = if ty == AssignNormal then do
    arr <- unwrapNoun (DomainError "Cannot print non-array") val
    out <- gets contextOut
    out $ show arr ++ "\n"
    return val 
    else throwError $ DomainError "Can only assign normally to quads"
  | name == [G.quadQuote] = if ty == AssignNormal then do
    arr <- unwrapNoun (DomainError "Cannot print non-array") val
    err <- gets contextErr
    err $ show arr
    return val
    else throwError $ DomainError "Can only assign normally to quads"
  | isPrefixOf [G.quad] name = if ty == AssignNormal then do
    arr <- unwrapNoun (DomainError "Cannot set quad name to non-array") val
    quads <- gets contextQuads
    let nilad = lookup name $ quadArrays quads
    case nilad of
      Just x -> setNilad x arr $> val
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
    else throwError $ DomainError "Can only assign normally to quads"
  | ty == AssignNormal = do
    sc <- gets contextScope
    readRef sc >>= scopeUpdate private name VariableNormal val >>= writeRef sc
    return val
  | ty == AssignConstant = do
    sc <- gets contextScope
    readRef sc >>= scopeUpdate private name VariableConstant val >>= writeRef sc
    return val
  | ty == AssignPrivate && private = do
    sc <- gets contextScope
    readRef sc >>= scopeUpdate True name VariablePrivate val >>= writeRef sc
    return val
  | ty == AssignPrivate && not private = throwError $ DomainError "Cannot access private variable"
  | ty == AssignModify && not shallow = do
    sc <- gets contextScope
    readRef sc >>= scopeModify private name val >>= writeRef sc
    return val
  | ty == AssignModify && shallow = do
    sc <- gets contextScope
    readRef sc >>= scopeShallowModify private name val >>= writeRef sc
    return val
  | otherwise = throwError unreachable

evalQualifiedAssign :: Value -> NonEmpty String -> AssignType -> Value -> St Value
evalQualifiedAssign head ns c val = do
  let err = DomainError "Qualified name head should be a scalar struct"
  let (q, l) = unsnocNE ns
  headCtx <- unwrapNoun err head >>= asScalar err >>= asStruct err
  ctx <- resolve headCtx q
  runWithContext ctx $ evalAssign True False l c val

evalVectorAssign :: [String] -> AssignType -> Value -> St Value
evalVectorAssign ns c val =
  if any (\(n:_) -> n == G.quad || n == G.quadQuote) ns then throwError $ DomainError "Vector assignment: cannot assign to quad names"
  else do
    es <- fmap fromScalar <$> (unwrapNoun (DomainError "Vector assign: not a vector") val >>= asVector (DomainError "Vector assignment: not a vector"))
    if length ns /= length es then throwError $ DomainError "Vector assignment: wrong number of names"
    else zipWithM_ (\name value -> evalAssign False True name c (VNoun value)) ns es $> val

evalHighRankAssign :: [String] -> AssignType -> Value -> St Value
evalHighRankAssign ns c val =
  if any (\(n:_) -> n == G.quad || n == G.quadQuote) ns then throwError $ DomainError "High rank assignment: cannot assign to quad names"
  else do
    es <- majorCells <$> unwrapNoun (DomainError "High rank assign: not an array") val
    if length ns /= length es then throwError $ DomainError "High rank assignment: wrong number of names"
    else zipWithM_ (\name value -> evalAssign False True name c (VNoun value)) ns es $> val

evalStructAssign :: [(String, Maybe (AssignType, String))] -> AssignType -> Value -> St Value
evalStructAssign ns c val = do
  let err = DomainError "Struct assignment: not a struct"
  s <- unwrapNoun err val >>= asScalar err >>= asStruct err >>= readRef . contextScope
  mapM_ (\(name, alias) -> do {
      let (n, t) = case alias of {
          Nothing -> (name, c)
        ; Just ((t', n')) -> (n', t') }
    ; let v = scopeShallowLookup False n s
    ; case v of
        Nothing -> throwError $ SyntaxError $ "Struct assignment: variable " ++ n ++ " does not exist"
        Just v' -> evalAssign False True name t v'
    ; pure () }) ns
  pure val

evalDefined :: NonEmpty Tree -> Category -> St Value
evalDefined statements cat = let
  ev :: Tree -> St (Value, Bool)
  ev (GuardBranch check result) = do
    c <- eval check >>= unwrapNoun (DomainError "Guard check not array") >>= lift . except . asScalar (DomainError "Guard check not scalar") >>= lift . except . asBool (DomainError "Guard check not boolean")
    if c then ev result
    else return (VNoun $ Array [0, 0] [], False)
  ev (ExitBranch result) = (, True) <$> eval result
  ev other = (, False) <$> eval other

  runDefined :: NonEmpty Tree -> St Value
  runDefined (x :| xs) = case NE.nonEmpty xs of
    Nothing -> fst <$> ev x
    Just xs -> do
      (v, r) <- ev x
      if r then return v else runDefined xs

  run xs sc = inChildScope xs (runDefined statements) sc >>= unwrapNoun (DomainError "Dfn must return an array")
  in do
    sc <- get
    case cat of
      CatFunction -> do
        id <- assignId
        let dfn = DefinedFunction {
            functionRepr = "{...}"
          , functionContext = Just $ sc
          , functionMonad = Just $ \x -> run [([G.omega], (VariableConstant, VNoun x)), ([G.del], (VariableConstant, VFunction dfn))] sc
          , functionDyad = Just $ \x y -> run [([G.alpha], (VariableConstant, VNoun x)), ([G.omega], (VariableConstant, VNoun y)), ([G.del], (VariableConstant, VFunction dfn))] sc
          , definedFunctionId = id }
        pure $ VFunction dfn
      CatAdverb -> do
        id <- assignId
        let dadv = DefinedAdverb {
            adverbRepr = "_{...}"
          , adverbContext = Just $ sc
          , adverbOnNoun = Just $ \a -> do
            id <- assignId
            let dfn = DefinedFunction {
                functionRepr = "(" ++ show a ++ ")_{...}"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alpha, G.alpha], (VariableConstant, VNoun a))
                , ([G.omega], (VariableConstant, VNoun x))
                , ([G.underscore, G.del], (VariableConstant, VAdverb dadv))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alpha, G.alpha], (VariableConstant, VNoun a))
                , ([G.alpha], (VariableConstant, VNoun x))
                , ([G.omega], (VariableConstant, VNoun y))
                , ([G.underscore, G.del], (VariableConstant, VAdverb dadv))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , definedFunctionId = id }
            pure dfn
          , adverbOnFunction = Just $ \a -> do
            id <- assignId
            let dfn = DefinedFunction {
                functionRepr = "(" ++ show a ++ ")_{...}"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alphaBar, G.alphaBar], (VariableConstant, VFunction a))
                , ([G.omega], (VariableConstant, VNoun x))
                , ([G.underscore, G.del], (VariableConstant, VAdverb dadv))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc           , functionDyad = Just $ \x y -> run
                [ ([G.alphaBar, G.alphaBar], (VariableConstant, VFunction a))
                , ([G.alpha], (VariableConstant, VNoun x))
                , ([G.omega], (VariableConstant, VNoun y))
                , ([G.underscore, G.del], (VariableConstant, VAdverb dadv))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , definedFunctionId = id }
            pure dfn
          , definedAdverbId = id }
        pure $ VAdverb dadv
      CatConjunction -> do
        id <- assignId
        let dconj = DefinedConjunction {
            conjRepr = "_{...}_"
          , conjContext = Just $ sc
          , conjOnNounNoun = Just $ \a b -> do
            id <- assignId
            let dfn = DefinedFunction {
                functionRepr = "(" ++ show a ++ ")_{...}_(" ++ show b ++ ")"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alpha, G.alpha], (VariableConstant, VNoun a))
                , ([G.omega, G.omega], (VariableConstant, VNoun b))
                , ([G.omega], (VariableConstant, VNoun x))
                , ([G.underscore, G.del, G.underscore], (VariableConstant, VConjunction dconj))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alpha, G.alpha], (VariableConstant, VNoun a))
                , ([G.omega, G.omega], (VariableConstant, VNoun b))
                , ([G.alpha], (VariableConstant, VNoun x))
                , ([G.omega], (VariableConstant, VNoun y))
                , ([G.underscore, G.del, G.underscore], (VariableConstant, VConjunction dconj))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , definedFunctionId = id }
            pure dfn
          , conjOnNounFunction = Just $ \a b -> do
            id <- assignId
            let dfn = DefinedFunction {
                functionRepr = "(" ++ show a ++ ")_{...}_(" ++ show b ++ ")"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alpha, G.alpha], (VariableConstant, VNoun a))
                , ([G.omegaBar, G.omegaBar], (VariableConstant, VFunction b))
                , ([G.omega], (VariableConstant, VNoun x))
                , ([G.underscore, G.del, G.underscore], (VariableConstant, VConjunction dconj))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alpha, G.alpha], (VariableConstant, VNoun a))
                , ([G.omegaBar, G.omegaBar], (VariableConstant, VFunction b))
                , ([G.alpha], (VariableConstant, VNoun x))
                , ([G.omega], (VariableConstant, VNoun y))
                , ([G.underscore, G.del, G.underscore], (VariableConstant, VConjunction dconj))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , definedFunctionId = id }
            pure dfn
          , conjOnFunctionNoun = Just $ \a b -> do
            id <- assignId
            let dfn = DefinedFunction {
                functionRepr = "(" ++ show a ++ ")_{...}_(" ++ show b ++ ")"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alphaBar, G.alphaBar], (VariableConstant, VFunction a))
                , ([G.omega, G.omega], (VariableConstant, VNoun b))
                , ([G.omega], (VariableConstant, VNoun x))
                , ([G.underscore, G.del, G.underscore], (VariableConstant, VConjunction dconj))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alphaBar, G.alphaBar], (VariableConstant, VFunction a))
                , ([G.omega, G.omega], (VariableConstant, VNoun b))
                , ([G.alpha], (VariableConstant, VNoun x))
                , ([G.omega], (VariableConstant, VNoun y))
                , ([G.underscore, G.del, G.underscore], (VariableConstant, VConjunction dconj))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , definedFunctionId = id }
            pure dfn
          , conjOnFunctionFunction = Just $ \a b -> do
            id <- assignId
            let dfn = DefinedFunction {
                functionRepr = "(" ++ show a ++ ")_{...}_(" ++ show b ++ ")"
              , functionContext = Just $ sc
              , functionMonad = Just $ \x -> run
                [ ([G.alphaBar, G.alphaBar], (VariableConstant, VFunction a))
                , ([G.omegaBar, G.omegaBar], (VariableConstant, VFunction b))
                , ([G.omega], (VariableConstant, VNoun x))
                , ([G.underscore, G.del, G.underscore], (VariableConstant, VConjunction dconj))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , functionDyad = Just $ \x y -> run
                [ ([G.alphaBar, G.alphaBar], (VariableConstant, VFunction a))
                , ([G.omegaBar, G.omegaBar], (VariableConstant, VFunction b))
                , ([G.alpha], (VariableConstant, VNoun x))
                , ([G.omega], (VariableConstant, VNoun y))
                , ([G.underscore, G.del, G.underscore], (VariableConstant, VConjunction dconj))
                , ([G.del], (VariableConstant, VFunction dfn)) ] sc
              , definedFunctionId = id }
            pure dfn
          , definedConjunctionId = id }
        pure $ VConjunction dconj
      cat -> throwError $ DomainError $ "Defined of type " ++ show cat ++ "?"

evalTrain :: Category -> [Maybe Tree] -> St Value
evalTrain cat es = let
  makeValueAdverb :: (Value -> St Function) -> String -> Adverb
  makeValueAdverb a s = PrimitiveAdverb
    { adverbOnNoun = Just $ \x -> a (VNoun x)
    , adverbOnFunction = Just $ \f -> a (VFunction f)
    , adverbRepr = s
    , adverbContext = Nothing }

  makeValueConjunction :: (Value -> Value -> St Function) -> String -> Conjunction
  makeValueConjunction a s = PrimitiveConjunction
    { conjOnNounNoun = Just $ \x y -> a (VNoun x) (VNoun y)
    , conjOnNounFunction = Just $ \x y -> a (VNoun x) (VFunction y)
    , conjOnFunctionNoun = Just $ \x y -> a (VFunction x) (VNoun y)
    , conjOnFunctionFunction = Just $ \x y -> a (VFunction x) (VFunction y)
    , conjRepr = s
    , conjContext = Nothing }

  atop :: Function -> Function -> Function
  atop f g = PrimitiveFunction { functionMonad = Just $ F.compose (callMonad f) (callMonad g), functionDyad = Just $ F.atop (callMonad f) (callDyad g), functionRepr = "", functionContext = Nothing }

  fork :: Function -> Function -> Function -> Function
  fork f g h = PrimitiveFunction { functionMonad = Just $ F.fork1 (callMonad f) (callDyad g) (callMonad h), functionDyad = Just $ F.fork2 (callDyad f) (callDyad g) (callDyad h), functionRepr = "", functionContext = Nothing }

  bindLeft :: Function -> Noun -> Function
  bindLeft f x = PrimitiveFunction { functionMonad = Just $ \y -> callDyad f x y, functionDyad = Nothing, functionRepr = "", functionContext = Nothing }

  bindRight :: Function -> Noun -> Function
  bindRight f y = PrimitiveFunction { functionMonad = Just $ \x -> callDyad f x y, functionDyad = Nothing, functionRepr = "", functionContext = Nothing }

  train1 :: Value -> St Value
  train1 (VNoun x) = pure $ VFunction $ PrimitiveFunction { functionMonad = Just $ F.constant1 x, functionDyad = Just $ F.constant2 x, functionRepr = "", functionContext = Nothing }
  train1 o = pure $ o

  train2 :: Value -> Value -> St Value
  train2 (VNoun x) (VNoun _) = pure $ VFunction $ PrimitiveFunction { functionMonad = Just $ F.constant1 x, functionDyad = Just $ F.constant2 x, functionRepr = "", functionContext = Nothing }
  train2 (VNoun x) (VFunction g) = pure $ VFunction $ PrimitiveFunction { functionMonad = Just $ \y -> callDyad g x y, functionDyad = Nothing, functionRepr = "", functionContext = Nothing}
  train2 (VFunction f) (VNoun y) = pure $ VFunction $ PrimitiveFunction { functionMonad = Just $ \x -> callDyad f x y, functionDyad = Nothing, functionRepr = "", functionContext = Nothing }
  train2 (VFunction f) (VFunction g) = pure $ VFunction $ atop f g
  train2 (VNoun x) (VAdverb a) = VFunction <$> callOnNoun a x 
  train2 (VFunction f) (VAdverb a) = VFunction <$> callOnFunction a f
  train2 (VAdverb a) (VFunction g) = pure $ VAdverb $ makeValueAdverb (\u -> (`atop` g) <$> callOnValue a u) ""
  train2 (VAdverb a) (VAdverb b) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValue a u >>= callOnFunction b) ""
  train2 (VAdverb a) (VConjunction c) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValue a u >>= (\r -> callOnValueAndValue c (VFunction r) u)) ""
  train2 x@(VNoun _) (VConjunction c) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValueAndValue c x u) ""
  train2 f@(VFunction _) (VConjunction c) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValueAndValue c f u) ""
  train2 (VConjunction c) y@(VNoun _) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValueAndValue c u y) ""
  train2 (VConjunction c) g@(VFunction _) = pure $ VAdverb $ makeValueAdverb (\u -> callOnValueAndValue c u g) ""
  train2 (VConjunction c) (VAdverb a) = pure $ VConjunction $ makeValueConjunction (\u v -> callOnValueAndValue c u v >>= callOnFunction a) ""
  train2 (VConjunction c) (VConjunction d) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    a <- callOnValueAndValue d u v
    b <- callOnValueAndValue c u v
    pure $ atop a b) ""
  train2 x y = throwError $ DomainError $ "2-train with " ++ show x ++ " and " ++ show y ++ "?"

  train3 :: Value -> Value -> Value -> St Value
  train3 (VNoun _) (VNoun y) (VNoun _) = pure $ VFunction $ PrimitiveFunction { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "", functionContext = Nothing }
  train3 (VNoun _) (VNoun y) (VFunction _) = pure $ VFunction $ PrimitiveFunction { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "", functionContext = Nothing }
  train3 (VFunction _) (VNoun y) (VNoun _) = pure $ VFunction $ PrimitiveFunction { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "", functionContext = Nothing }
  train3 (VFunction _) (VNoun y) (VFunction _) = pure $ VFunction $ PrimitiveFunction { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "", functionContext = Nothing }
  train3 (VFunction f) (VFunction g) (VFunction h) = pure $ VFunction $ fork f g h
  train3 (VNoun x) (VFunction g) (VFunction h) = pure $ VFunction $ atop (bindLeft g x) h
  train3 (VFunction f) (VFunction g) (VNoun z) = pure $ VFunction $ atop (bindRight g z) f
  train3 (VNoun x) (VFunction g) (VNoun z) = do
    r <- callDyad g x z
    pure $ VFunction $ PrimitiveFunction { functionMonad = Just $ F.constant1 r, functionDyad = Just $ F.constant2 r, functionRepr = "", functionContext = Nothing }
  train3 (VNoun x) (VConjunction c) (VNoun z) = VFunction <$> callOnNounAndNoun c x z
  train3 (VNoun x) (VConjunction c) (VFunction h) = VFunction <$> callOnNounAndFunction c x h
  train3 (VFunction f) (VConjunction c) (VNoun z) = VFunction <$> callOnFunctionAndNoun c f z
  train3 (VFunction f) (VConjunction c) (VFunction h) = VFunction <$> callOnFunctionAndFunction c f h
  train3 (VAdverb a) (VFunction g) (VFunction h) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    pure $ fork r g h) ""
  train3 (VAdverb a) (VAdverb b) (VAdverb c) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    s <- callOnFunction b r
    callOnFunction c s) ""
  train3 (VNoun x) (VConjunction c) (VAdverb a) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    callOnNounAndFunction c x r) ""
  train3 (VFunction f) (VConjunction c) (VAdverb a) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    callOnFunctionAndFunction c f r) ""
  train3 (VAdverb a) (VConjunction c) (VNoun z) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    callOnFunctionAndNoun c r z) ""
  train3 (VAdverb a) (VConjunction c) (VFunction h) = pure $ VAdverb $ makeValueAdverb (\u -> do
    r <- callOnValue a u
    callOnFunctionAndFunction c r h) ""
  train3 (VFunction f) (VFunction g) (VConjunction c) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue c u v
    pure $ fork f g r) ""
  train3 (VNoun x) (VFunction g) (VConjunction c) = pure $ VConjunction $ makeValueConjunction (\u v -> do
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
  train3 (VNoun x) (VConjunction c) (VConjunction d) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue d u v
    callOnNounAndFunction c x r) ""
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
  train3 (VConjunction c) (VConjunction d) (VNoun z) = pure $ VConjunction $ makeValueConjunction (\u v -> do
    r <- callOnValueAndValue c u v
    callOnFunctionAndNoun d r z) ""
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

  withTrainRepr :: [Maybe Value] -> Value -> St Value
  withTrainRepr _ (VNoun _) = throwError $ DomainError "Array train?"
  withTrainRepr us (VFunction f) = pure $ VFunction $ TrainFunction { functionMonad = functionMonad f, functionDyad = functionDyad f, functionContext = functionContext f, trainFunctionTines = us }
  withTrainRepr us (VAdverb a) = let a'' = TrainAdverb {
      adverbOnNoun = (\a' x -> (\fn -> DerivedFunctionNoun { functionMonad = functionMonad fn, functionDyad = functionDyad fn, functionContext = functionContext fn, derivedFunctionAdverb = a'', derivedFunctionNounLeft = x }) <$> a' x) <$> adverbOnNoun a
    , adverbOnFunction = (\a' f -> (\fn -> DerivedFunctionFunction { functionMonad = functionMonad fn, functionDyad = functionDyad fn, functionContext = functionContext fn, derivedFunctionAdverb = a'', derivedFunctionFunctionLeft = f }) <$> a' f) <$> adverbOnFunction a
    , adverbContext = adverbContext a
    , trainAdverbTines = us }
    in pure $ VAdverb a''
  withTrainRepr us (VConjunction c) = let c'' = TrainConjunction {
      conjOnNounNoun = (\c' x y -> (\fn -> DerivedFunctionNounNoun { functionMonad = functionMonad fn, functionDyad = functionDyad fn, functionContext = functionContext fn, derivedFunctionConjunction = c'', derivedFunctionNounLeft = x, derivedFunctionNounRight = y }) <$> c' x y) <$> conjOnNounNoun c
    , conjOnNounFunction = (\c' x y -> (\fn -> DerivedFunctionNounFunction { functionMonad = functionMonad fn, functionDyad = functionDyad fn, functionContext = functionContext fn, derivedFunctionConjunction = c'', derivedFunctionNounLeft = x, derivedFunctionFunctionRight = y }) <$> c' x y) <$> conjOnNounFunction c
    , conjOnFunctionNoun = (\c' f y -> (\fn -> DerivedFunctionFunctionNoun { functionMonad = functionMonad fn, functionDyad = functionDyad fn, functionContext = functionContext fn, derivedFunctionConjunction = c'', derivedFunctionFunctionLeft = f, derivedFunctionNounRight = y }) <$> c' f y) <$> conjOnFunctionNoun c
    , conjOnFunctionFunction = (\c' f g -> (\fn -> DerivedFunctionFunctionFunction { functionMonad = functionMonad fn, functionDyad = functionDyad fn, functionContext = functionContext fn, derivedFunctionConjunction = c'', derivedFunctionFunctionLeft = f, derivedFunctionFunctionRight = g }) <$> c' f g) <$> conjOnFunctionFunction c
    , conjContext = conjContext c
    , trainConjunctionTines = us }
    in pure $ VConjunction c''
  in do
    us <- mapM (\case
      Nothing -> pure Nothing
      Just x -> Just <$> eval x) $ reverse es
    t <- train us
    r <- withTrainRepr (reverse us) t
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
  pure $ VNoun $ scalar $ Struct newContext

evalUnwrap :: Category -> Value -> St Value
evalUnwrap CatFunction v = do
  let err = DomainError "Unwrap notation: array of wraps required"
  VFunction <$> (unwrapNoun err v >>= asWraps err)
evalUnwrap CatAdverb v = do
  let err = DomainError "Unwrap adverb notation: scalar array wrap required"
  arr <- unwrapNoun err v
  if null $ arrayShape arr then VAdverb <$> asAdverbWrap err (headPromise $ arrayContents arr)
  else throwError err
evalUnwrap CatConjunction v = do
  let err = DomainError "Unwrap conjunction notation: scalar array wrap required"
  arr <- unwrapNoun err v
  if null $ arrayShape arr then VConjunction <$> asConjunctionWrap err (headPromise $ arrayContents arr)
  else throwError err
evalUnwrap _ _ = throwError unreachable
