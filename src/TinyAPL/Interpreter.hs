{-# LANGUAGE TupleSections, LambdaCase #-}
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
  , adverbRepr = s }

makeValueConjunction :: (Value -> Value -> St Function) -> String -> Conjunction
makeValueConjunction a s = Conjunction
  { conjOnArrayArray = Just $ \x y -> a (VArray x) (VArray y)
  , conjOnArrayFunction = Just $ \x y -> a (VArray x) (VFunction y)
  , conjOnFunctionArray = Just $ \x y -> a (VFunction x) (VArray y)
  , conjOnFunctionFunction = Just $ \x y -> a (VFunction x) (VFunction y)
  , conjRepr = s } 

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
eval (GuardBranch _ _)              = throwError $ DomainError "Guards are not allowed outside of dfns"
eval (ExitBranch _)                 = throwError $ DomainError "Exits are not allowed outside of dfns"
eval (VectorBranch es)              = do
  entries <- mapM (eval >=> unwrapArray (DomainError "Array notation entries must be arrays")) es
  return $ VArray $ vector $ box <$> entries
eval (HighRankBranch es)            = do
  entries <- mapM (eval >=> unwrapArray (DomainError "Array notation entries must be arrays")) es
  case entries of
    [] -> return $ VArray $ fromMajorCells entries
    (e:es) ->
      if all ((== arrayShape e) . arrayShape) es
      then return $ VArray $ fromMajorCells entries
      else throwError $ DomainError "High rank notation entries must be of the same shape"
eval (TrainBranch cat es)           = evalTrain cat es

evalLeaf :: Token -> St Value
evalLeaf (TokenNumber [x] _)           = return $ VArray $ scalar $ Number x
evalLeaf (TokenNumber xs _)            = return $ VArray $ vector $ Number <$> xs
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

evalTrain :: Category -> [Maybe Tree] -> St Value
evalTrain cat es = let
  atop :: Function -> Function -> Function
  atop f g = Function { functionMonad = Just $ F.compose (callMonad f) (callMonad g), functionDyad = Just $ F.atop (callMonad f) (callDyad g), functionRepr = "" }

  fork :: Function -> Function -> Function -> Function
  fork f g h = Function { functionMonad = Just $ F.fork1 (callMonad f) (callDyad g) (callMonad h), functionDyad = Just $ F.fork2 (callDyad g) (callDyad f) (callDyad h), functionRepr = "" }

  bindLeft :: Function -> Array -> Function
  bindLeft f x = Function { functionMonad = Just $ \y -> callDyad f x y, functionDyad = Nothing, functionRepr = "" }

  bindRight :: Function -> Array -> Function
  bindRight f y = Function { functionMonad = Just $ \x -> callDyad f x y, functionDyad = Nothing, functionRepr = "" }

  train1 :: Value -> St Value
  train1 (VArray x) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 x, functionDyad = Just $ F.constant2 x, functionRepr = "" }
  train1 o = pure $ o

  train2 :: Value -> Value -> St Value
  train2 (VArray x) (VArray _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 x, functionDyad = Just $ F.constant2 x, functionRepr = "" }
  train2 (VArray x) (VFunction g) = pure $ VFunction $ Function { functionMonad = Just $ \y -> callDyad g x y, functionDyad = Nothing, functionRepr = ""}
  train2 (VFunction f) (VArray y) = pure $ VFunction $ Function { functionMonad = Just $ \x -> callDyad f x y, functionDyad = Nothing, functionRepr = "" }
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
  train3 (VArray _) (VArray y) (VArray _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "" }
  train3 (VArray _) (VArray y) (VFunction _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "" }
  train3 (VFunction _) (VArray y) (VArray _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "" }
  train3 (VFunction _) (VArray y) (VFunction _) = pure $ VFunction $ Function { functionMonad = Just $ F.constant1 y, functionDyad = Just $ F.constant2 y, functionRepr = "" }
  train3 (VFunction f) (VFunction g) (VFunction h) = pure $ VFunction $ fork f g h
  train3 (VArray x) (VFunction g) (VFunction h) = pure $ VFunction $ atop (bindLeft g x) h
  train3 (VFunction f) (VFunction g) (VArray z) = pure $ VFunction $ atop (bindRight g z) f
  train3 (VArray x) (VFunction g) (VArray z) = do
    r <- callDyad g x z
    pure $ VFunction $ Function { functionMonad = Just $ F.constant1 r, functionDyad = Just $ F.constant2 r, functionRepr = "" }
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

  withTrainRepr :: [Maybe Value] -> Value -> St Value
  withTrainRepr _ (VArray _) = throwError $ DomainError "Array train?"
  withTrainRepr us (VFunction f) = pure $ VFunction $ f{ functionRepr = [fst G.train] ++ intercalate [' ', G.separator, ' '] (show <$> us) ++ [snd G.train] }
  withTrainRepr us (VAdverb a) = pure $ VAdverb $ a{ adverbRepr = [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] (show <$> us) ++ [snd G.train] }
  withTrainRepr us (VConjunction c) = pure $ VConjunction $ c{ conjRepr = [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] (show <$> us) ++ [snd G.train, G.underscore] }
  in do
    us <- mapM (\case
      Nothing -> pure Nothing
      Just x -> Just <$> eval x) es
    t <- train $ reverse us
    r <- withTrainRepr us t
    case (cat, r) of
      (CatArray, _) -> throwError $ DomainError "Array train?"
      (CatFunction, r@(VFunction _)) -> pure r
      (CatAdverb, r@(VAdverb _)) -> pure r
      (CatConjunction, r@(VConjunction _)) -> pure r
      (exp, g) -> throwError $ DomainError $ "Expected train of category " ++ show exp ++ ", got a " ++ show (valueCategory g)
