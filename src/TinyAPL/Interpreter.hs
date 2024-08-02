{-# LANGUAGE TupleSections #-}
module TinyAPL.Interpreter where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import TinyAPL.Parser
import qualified TinyAPL.Primitives as P
import TinyAPL.Util

import Control.Applicative ((<|>))
import Control.Monad.State
import Data.Foldable (foldlM)
import Control.Monad

data Value
  = VArray Array
  | VFunction Function
  | VAdverb Adverb
  | VConjunction Conjunction

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

scopeLookup :: String -> Scope -> Maybe Value
scopeLookup name sc = (VArray <$> scopeLookupArray name sc)
                  <|> (VFunction <$> scopeLookupFunction name sc)
                  <|> (VAdverb <$> scopeLookupAdverb name sc)
                  <|> (VConjunction <$> scopeLookupConjunction name sc)

scopeUpdate :: String -> Value -> Scope -> Scope
scopeUpdate name (VArray val) sc       = scopeUpdateArray name val sc
scopeUpdate name (VFunction val) sc    = scopeUpdateFunction name val sc
scopeUpdate name (VAdverb val) sc      = scopeUpdateAdverb name val sc
scopeUpdate name (VConjunction val) sc = scopeUpdateConjunction name val sc

inChildScope :: Monad m => [(String, Value)] -> StateT Context m a -> Context -> m a
inChildScope vals x parent = let
  child = parent{ contextScope = foldr (\(name, val) sc -> scopeUpdate name val sc) (Scope [] [] [] [] (Just $ contextScope parent)) vals }
  in evalStateT x child

interpret :: Tree -> Context -> ResultIO (Value, Context)
interpret tree = runSt (eval tree)

run :: String -> String -> Context -> ResultIO (Value, Context)
run file src scope = do
  trees <- except (parse file src)
  join $ foldlM (\last next -> interpret next . snd <$> last) (return (undefined, scope)) trees

eval :: Tree -> St Value
eval (Leaf _ tok)                   = evalLeaf tok
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
eval (AssignBranch _ n val)         = eval val >>= evalAssign n
eval (DefinedBranch cat statements) = evalDefined statements cat
eval (VectorBranch es)              = do
  entries <- mapM (eval >=> unwrapArray (DomainError "Array notation entries must be arrays")) es
  return $ VArray $ vector $ box <$> entries
eval (HighRankBranch es)            = do
  entries <- mapM (eval >=> unwrapArray (DomainError "Array notation entries must be arrays")) es
  return $ VArray $ fromMajorCells entries
eval _                              = throwError $ DomainError "Invalid branch in evaluation"

evalLeaf :: Token -> St Value
evalLeaf (TokenNumber num _)           = return $ VArray $ scalar $ Number num
evalLeaf (TokenChar [x] _)             = return $ VArray $ scalar $ Character x
evalLeaf (TokenChar xs _)              = return $ VArray $ vector $ Character <$> xs
evalLeaf (TokenString xs _)            = return $ VArray $ vector $ Character <$> xs
evalLeaf (TokenPrimArray n _)          =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive array " ++ [n]) $ VArray <$> lookup n P.arrays
evalLeaf (TokenPrimFunction n _)       =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive function " ++ [n]) $ VFunction <$> lookup n P.functions
evalLeaf (TokenPrimAdverb n _)         =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive adverb " ++ [n]) $ VAdverb <$> lookup n P.adverbs
evalLeaf (TokenPrimConjunction n _)    =
  lift $ except $ maybeToEither (SyntaxError $ "Unknown primitive conjunction " ++ [n]) $ VConjunction <$> lookup n P.conjunctions
evalLeaf (TokenArrayName name _)
  | name == [G.quad]                   = do
    out <- gets contextOut
    input <- gets contextIn
    out $ G.quad : ": "
    code <- input
    context <- get
    (res, context') <- lift $ run [G.quad] code context
    put $ context'
    return res
  | name == [G.quadQuote]              = do
    input <- gets contextIn
    str <- input
    return $ VArray $ vector $ Character <$> str
  | head name == G.quad                = do
    quads <- gets contextQuads
    let nilad = lookup name $ quadArrays quads
    case nilad of
      Just x -> case niladGet x of
        Just g -> VArray <$> g
        Nothing -> throwError $ SyntaxError $ "Quad name " ++ name ++ " cannot be accessed"
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise                          =
    gets contextScope >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VArray . scopeLookupArray name)
evalLeaf (TokenFunctionName name _)
  | head name == G.quad                = do
    quads <- gets contextQuads
    let fn = lookup name $ quadFunctions quads
    case fn of
      Just x -> return $ VFunction x
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise                          =
    gets contextScope >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VFunction . scopeLookupFunction name)
evalLeaf (TokenAdverbName name _)
  | head name == G.quad                = do
    quads <- gets contextQuads
    let adv = lookup name $ quadAdverbs quads
    case adv of
      Just x -> return $ VAdverb x
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise                          =
    gets contextScope >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VAdverb . scopeLookupAdverb name)
evalLeaf (TokenConjunctionName name _)
  | head name == G.quad                = do
    quads <- gets contextQuads
    let conj = lookup name $ quadConjunctions quads
    case conj of
      Just x -> return $ VConjunction x
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise                          =
    gets contextScope >>= (lift . except . maybeToEither (SyntaxError $ "Variable " ++ name ++ " does not exist") . fmap VConjunction . scopeLookupConjunction name)
evalLeaf _                             = throwError $ DomainError "Invalid leaf type in evaluation"

evalMonadCall :: Value -> Value -> St Value
evalMonadCall (VFunction fn) (VArray arr) = VArray <$> callMonad fn arr
evalMonadCall _ _                         = throwError $ DomainError "Invalid arguments to monad call evaluation"

evalDyadCall :: Value -> Value -> St Value
evalDyadCall (VArray arr) (VFunction f) =
  return $ VFunction $ Function { functionMonad = Just $ callDyad f arr, functionDyad = Nothing, functionRepr = "(" ++ show arr ++ show f ++ ")" }
evalDyadCall _ _                        = throwError $ DomainError "Invalid arguments to dyad call evaluation"

evalAdverbCall :: Value -> Value -> St Value
evalAdverbCall (VArray l) (VAdverb adv)    = VFunction <$> callOnArray adv l
evalAdverbCall (VFunction l) (VAdverb adv) = VFunction <$> callOnFunction adv l
evalAdverbCall _ _                         = throwError $ DomainError "Invalid arguments to adverb call evaluation"

evalConjunctionCall :: Value -> Value -> St Value
evalConjunctionCall (VConjunction conj) (VArray r)    =
  return $ VAdverb $ Adverb { adverbOnArray = Just (\x -> callOnArrayAndArray conj x r), adverbOnFunction = Just (\x -> callOnFunctionAndArray conj x r), adverbRepr = "(" ++ show conj ++ show r ++ ")" }
evalConjunctionCall (VConjunction conj) (VFunction r) =
  return $ VAdverb $ Adverb { adverbOnArray = Just (\x -> callOnArrayAndFunction conj x r), adverbOnFunction = Just (\x -> callOnFunctionAndFunction conj x r), adverbRepr = "(" ++ show conj ++ show r ++ ")" }
evalConjunctionCall _ _                               = throwError $ DomainError "Invalid arguments to conjunction call evaluation"

evalAssign :: String -> Value -> St Value
evalAssign name val
  | name == [G.quad]      = do
    arr <- unwrapArray (DomainError "Cannot print non-array") val
    out <- gets contextOut
    out $ show arr ++ "\n"
    return val
  | name == [G.quadQuote] = do
    arr <- unwrapArray (DomainError "Cannot print non-array") val
    err <- gets contextErr
    err $ show arr
    return val
  | head name == G.quad   = do
    arr <- unwrapArray (DomainError "Cannot set quad name to non-array") val
    quads <- gets contextQuads
    let nilad = lookup name $ quadArrays quads
    case nilad of
      Just x -> case niladSet x of
        Just s -> do
          s arr
          return val
        Nothing -> throwError $ SyntaxError $ "Quad name " ++ name ++ " cannot be set"
      Nothing -> throwError $ SyntaxError $ "Unknown quad name " ++ name
  | otherwise = do
    sc <- gets contextScope
    putScope $ scopeUpdate name val sc
    return val

evalDefined :: [Tree] -> Category -> St Value
evalDefined statements cat = let
  ev :: Tree -> St (Value, Bool)
  ev (GuardBranch check result) = do
    c <- eval check >>= unwrapArray (DomainError "Guard check not array") >>= lift . except . asScalar (DomainError "Guard check not scalar") >>= lift . except . asBool (DomainError "Guard check not boolean")
    if c then ev result
    else return (VArray $ Array [0, 0] [], False)
  ev (ExitBranch result) = (, True) <$> eval result
  ev other = (, False) <$> eval other

  runDefined :: [Tree] -> St Value
  runDefined [] = throwError $ DomainError "Eval empty dfn/dadv/dconj"
  runDefined [x] = fst <$> ev x
  runDefined (x:xs) = do
    (v, r) <- ev x
    if r then return v else runDefined xs

  run xs sc = lift (inChildScope xs (runDefined statements) sc) >>= unwrapArray (DomainError "Dfn must return an array")
  in do
    sc <- get
    case cat of
      CatFunction -> let
        dfn = VFunction (Function
          { functionRepr = "{...}"
          , functionMonad = Just $ \x -> run [([G.omega], VArray x), ([G.del], dfn)] sc
          , functionDyad = Just $ \x y -> run [([G.alpha], VArray x), ([G.omega], VArray y), ([G.del], dfn)] sc } )
        in return dfn
      CatAdverb -> let
        dadv = VAdverb (Adverb
          { adverbRepr = "_{...}"
          , adverbOnArray = Just $ \a -> let
            dfn = (Function
              { functionRepr = "(" ++ show a ++ ")_{...}"
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
          , conjOnArrayArray = Just $ \a b -> let
            dfn = (Function
              { functionRepr = "(" ++ show a ++ ")_{...}_(" ++ show b ++ ")"
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