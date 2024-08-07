{-# LANGUAGE FlexibleContexts, LambdaCase #-}
module TinyAPL.ArrayFunctionOperator where

import TinyAPL.Error
import TinyAPL.Util
import qualified TinyAPL.Glyphs as G
import Data.Complex ( magnitude, realPart, Complex(..) )
import Numeric.Natural
import Data.List
import Control.Monad
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad.State
import Control.Monad.Except (ExceptT, MonadError)
import Control.Applicative (Alternative((<|>)))
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Tuple (swap)

-- * Arrays

{-|
  Scalars:
   * complex numbers (internally represented as @Complex Double@s)
   * characters
   * array boxes (enclosures)
-}
data ScalarValue
  = Number (Complex Double)
  | Character Char
  | Box Array

data Array = Array
  { arrayShape :: [Natural]
  , arrayContents :: [ScalarValue] }

-- * Array helper functions

arrayRank :: Array -> Natural
arrayRank (Array sh _) = genericLength sh

box :: Array -> ScalarValue
box b@(Array [] [Box _]) = Box b
box (Array [] [x]) = x
box arr = Box arr

fromScalar :: ScalarValue -> Array
fromScalar (Box arr) = arr
fromScalar sc        = scalar sc

toScalar :: Array -> ScalarValue
toScalar (Array [] [x]) = x
toScalar arr            = box arr

scalar :: ScalarValue -> Array
scalar x = Array [] [x]

vector :: [ScalarValue] -> Array
vector xs = Array [genericLength xs] xs

arrayOf :: [Natural] -> [ScalarValue] -> Maybe Array
arrayOf sh cs
  | product sh == genericLength cs = Just $ Array sh cs
  | otherwise = Nothing

arrayReshaped :: [Natural] -> [ScalarValue] -> Maybe Array
arrayReshaped sh cs =
  if null cs then
    if 0 `elem` sh
    then Just $ Array sh []
    else Nothing
  else Just $ Array sh $ genericTake (product sh) $ cycle cs

arrayReshapedNE :: [Natural] -> NonEmpty ScalarValue -> Array
arrayReshapedNE sh cs = fromJust $ arrayReshaped sh $ toList cs

majorCells :: Array -> [Array]
majorCells a@(Array [] _) = [a]
majorCells (Array (_:sh) cs) = mapMaybe (arrayOf sh) $ chunk (product sh) cs where
  chunk _ [] = []
  chunk l xs = genericTake l xs : chunk l (genericDrop l xs)

fromMajorCells :: [Array] -> Array
fromMajorCells [] = Array [0] []
fromMajorCells (c:cs) = fromJust $ arrayReshaped (1 + genericLength cs : arrayShape c) $ concatMap arrayContents $ c : cs

-- * Number comparison functions

comparisonTolerance :: Double
comparisonTolerance = 1e-14

realEqual :: Double -> Double -> Bool
realEqual a b = abs (a - b) <= comparisonTolerance * (abs a `max` abs b)
complexEqual :: Complex Double -> Complex Double -> Bool
complexEqual a b = magnitude (a - b) <= comparisonTolerance * (magnitude a `max` magnitude b)

isReal :: Complex Double -> Bool
isReal (_ :+ b) = 0 `realEqual` b -- A number is real if its imaginary part compares equal to zero.

-- * Total ordering for scalars and arrays

instance Eq ScalarValue where
  (Character a) == (Character b) = a == b
  (Box as) == (Box bs) = as == bs
  (Number a) == (Number b)
    | isReal a && isReal b = realPart a `realEqual` realPart b
    | otherwise = a `complexEqual` b
  _ == _ = False

{-|
  Order:
   * numbers, in lexicographical order (real then imaginary)
   * characters, in codepoint order
   * boxes, ordered by their contents
-}
instance Ord ScalarValue where
  (Number (ar :+ ai)) `compare` (Number (br :+ bi))
    | ar `realEqual` br && ai `realEqual` bi = EQ
    | ar `realEqual` br = ai `compare` bi
    | otherwise = ar `compare` br
  (Number _) `compare` _ = LT
  (Character _) `compare` (Number _) = GT
  (Character a) `compare` (Character b) = a `compare` b
  (Character _) `compare` _ = LT
  (Box as) `compare` (Box bs) = as `compare` bs
  (Box _) `compare` _ = GT

instance Eq Array where
  -- Two arrays are equal iff both their shapes and their ravels are equal.
  (Array ash as) == (Array bsh bs) = (ash, as) == (bsh, bs)

instance Ord Array where
  -- Arrays are ordered by shape and then contents
  (Array ash as) `compare` (Array bsh bs) = (ash `compare` bsh) <> (as `compare` bs)

isInt :: Double -> Bool
isInt = realEqual <*> (fromInteger . floor)

-- * @Show@ for scalars and arrays

showComplex :: Complex Double -> String
showComplex (a :+ b)
  | b `realEqual` 0 = showAplDouble a
  | otherwise = showAplDouble a ++ [G.imaginary] ++ showAplDouble b

instance Show ScalarValue where
  show (Number x) = showComplex x
  show (Character x) = [x]
  show (Box xs) = G.enclose : show xs

instance Show Array where
  show (Array [] [s])                                                   = show s
  show (Array [_] xs)
    | not (null xs) && all (\case (Character _) -> True; _ -> False) xs = xs >>= show
    | otherwise                                                         = [fst G.vector] ++ intercalate [' ', G.separator, ' '] (show . fromScalar <$> xs) ++ [snd G.vector]
  show arr                                                              = [fst G.highRank] ++ intercalate [' ', G.separator, ' '] (show <$> majorCells arr) ++ [snd G.highRank]

charRepr :: Char -> (String, Bool)
charRepr c = case lookup c (swap <$> G.escapes) of
  Just e -> ([G.stringEscape, e], True)
  Nothing -> ([c], False)

scalarRepr :: ScalarValue -> String
scalarRepr (Number x) = showComplex x
scalarRepr (Character x) = case charRepr x of
  (e, True) -> [G.first, G.stringDelimiter] ++ e ++ [G.stringDelimiter]
  (c, False) -> [G.charDelimiter] ++ c ++ [G.charDelimiter]
scalarRepr (Box xs) = G.enclose : arrayRepr xs

stringRepr :: [Char] -> String
stringRepr str = [G.stringDelimiter] ++ concatMap (fst . charRepr) str ++ [G.stringDelimiter]

arrayRepr :: Array -> String
arrayRepr (Array [] [s]) = scalarRepr s
arrayRepr (Array [_] xs)
  | not (null xs) && all (\case (Character _) -> True; _ -> False) xs = stringRepr $ (\case (Character c) -> c; _ -> undefined) <$> xs
  | otherwise = [fst G.vector] ++ intercalate [' ', G.separator, ' '] (arrayRepr . fromScalar <$> xs) ++ [snd G.vector]
arrayRepr arr = [fst G.highRank] ++ intercalate [' ', G.separator, ' '] (arrayRepr <$> majorCells arr) ++ [snd G.highRank]

-- * Conversions

boolToScalar :: Bool -> ScalarValue
boolToScalar True = Number 1
boolToScalar False = Number 0

asBool :: MonadError Error m => Error -> ScalarValue -> m Bool
asBool _ (Number 0) = pure False
asBool _ (Number 1) = pure True
asBool e _ = throwError e

asNumber :: MonadError Error m => Error -> ScalarValue -> m (Complex Double)
asNumber _ (Number x) = pure x
asNumber e _ = throwError e

asReal :: MonadError Error m => Error -> Complex Double -> m Double
asReal e x
  | isReal x = pure $ realPart x
  | otherwise = throwError e

asInt' :: MonadError Error m => Integral num => Error -> Double -> m num
asInt' e x
  | isInt x = pure $ fromInteger $ floor x
  | otherwise = throwError e

asInt :: (MonadError Error m, Integral num) => Error -> Complex Double -> m num
asInt e = asInt' e <=< asReal e

asNat' :: (MonadError Error m, Integral num) => Error -> num -> m Natural
asNat' e x
  | x >= 0 = pure $ toEnum $ fromEnum x
  | otherwise = throwError e

asNat :: MonadError Error m => Error -> Complex Double -> m Natural
asNat e = asNat' e <=< asInt e

isScalar :: Array -> Bool
isScalar (Array [] _) = True
isScalar _ = False

asScalar :: MonadError Error m => Error -> Array -> m ScalarValue
asScalar _ (Array _ [x]) = pure x
asScalar e _ = throwError e

isEmpty :: Array -> Bool
isEmpty (Array sh _) = 0 `elem` sh

asVector :: MonadError Error m => Error -> Array -> m [ScalarValue]
asVector _ (Array [] scalar) = pure scalar
asVector _ (Array [_] vec)   = pure vec
asVector e _                 = throwError e

onMajorCells :: MonadError Error m =>
  ([Array] -> m [Array])
  -> Array -> m Array
onMajorCells f x = do
  result <- f $ majorCells x
  case arrayReshaped (arrayShape x) $ concatMap arrayContents result of
    Nothing -> throwError $ DomainError ""
    Just rs -> return rs

-- * Scalar functions

scalarMonad :: MonadError Error m =>
  (ScalarValue -> m ScalarValue)
      -> Array -> m Array
scalarMonad f (Array sh cs) = Array sh <$> mapM f' cs where
  f' (Box xs) = Box <$> scalarMonad f xs
  f' x = f x

scalarDyad :: MonadError Error m =>
  (ScalarValue -> ScalarValue -> m ScalarValue)
      -> Array ->       Array -> m Array
scalarDyad f a@(Array ash as) b@(Array bsh bs)
  | isScalar a && isScalar b = let ([a'], [b']) = (as, bs) in scalar <$> f' a' b'
  | isScalar a = let [a'] = as in Array bsh <$> mapM (a' `f'`) bs
  | isScalar b = let [b'] = bs in Array ash <$> mapM (`f'` b') as
  | ash == bsh =
    Array (arrayShape a) <$> zipWithM f' (arrayContents a) (arrayContents b)
  | length ash /= length bsh = throwError $ RankError "Mismatched left and right argument ranks"
  | otherwise = throwError $ LengthError "Mismatched left and right argument shapes"
  where
    f' (Box as) (Box bs) = Box <$> scalarDyad f as bs
    f' (Box as) b = Box <$> scalarDyad f as (scalar b)
    f' a (Box bs) = Box <$> scalarDyad f (scalar a) bs
    f' a b = f a b

-- * Instances for arrays

monadN2N f = scalarMonad f' where
  f' x = do
    x' <- flip asNumber x $ DomainError "Expected number"
    Number <$> f x'

monadN2N' f = monadN2N $ pure . f

dyadNN2N f = scalarDyad f' where
  f' a b = do
    a' <- flip asNumber a $ DomainError "Expected number"
    b' <- flip asNumber b $ DomainError "Expected number"
    Number <$> f a' b'

dyadNN2N' f = dyadNN2N $ pure .: f

monadB2B f = scalarMonad f' where
  f' x = do
    x' <- flip asBool x $ DomainError "Expected boolean"
    boolToScalar <$> f x'

monadB2B' f = monadB2B $ pure . f

dyadBB2B f = scalarDyad f' where
  f' a b = do
    a' <- flip asBool a $ DomainError "Expected boolean"
    b' <- flip asBool b $ DomainError "Expected boolean"
    boolToScalar <$> f a' b'

dyadBB2B' f = dyadBB2B $ pure .: f

instance Num Array where
  (+) = unerror .: dyadNN2N' (+)
  (-) = unerror .: dyadNN2N' (-)
  (*) = unerror .: dyadNN2N' (*)
  abs = unerror . monadN2N' abs
  signum = unerror . monadN2N' signum
  fromInteger = scalar . Number . fromInteger

instance Fractional Array where
  recip = unerror . monadN2N (\case
    0 -> throwError $ DomainError "Divide by zero"
    x -> pure $ recip x)
  (/) = unerror .: dyadNN2N (\cases
    0 0 -> pure 1
    _ 0 -> throwError $ DomainError "Divide by zero"
    x y -> pure $ x / y)
  fromRational = scalar . Number . fromRational

instance Floating Array where
  pi = scalar $ Number pi
  exp = unerror . monadN2N' exp
  log = unerror . monadN2N (\case
    0 -> throwError $ DomainError "Logarithm of zero"
    x -> pure $ log x)
  sin = unerror . monadN2N' sin
  cos = unerror . monadN2N' cos
  tan = unerror . monadN2N' tan
  asin = unerror . monadN2N' asin
  acos = unerror . monadN2N' acos
  atan = unerror . monadN2N' atan
  sinh = unerror . monadN2N' sinh
  cosh = unerror . monadN2N' cosh
  tanh = unerror . monadN2N' tanh
  asinh = unerror . monadN2N' asinh
  acosh = unerror . monadN2N' acosh
  atanh = unerror . monadN2N' atanh

-- * Functions

data Function
  = Function
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionRepr  :: String }

makeAdverbRepr :: String -> Char -> String
makeAdverbRepr l s = "(" ++ l ++ ")" ++ [s]

makeConjRepr :: String -> Char -> String -> String
makeConjRepr l s r = "(" ++ l ++ ")" ++ [s] ++ "(" ++ r ++ ")"

instance Show Function where
  show (Function { functionRepr = repr }) = repr

noMonad :: String -> Error
noMonad str = DomainError $ "Function " ++ str ++ " cannot be called monadically"

callMonad :: Function -> Array -> St Array
callMonad (Function (Just f) _ _) x = f x
callMonad f@(Function Nothing _ _) _ = throwError $ noMonad $ show f

noDyad :: String -> Error
noDyad str = DomainError $ "Function " ++ str ++ " cannot be called dyadically"

callDyad :: Function -> Array -> Array -> St Array
callDyad (Function _ (Just g) _) a b = g a b
callDyad f@(Function _ Nothing _) _ _ = throwError $ noDyad $ show f

-- * Operators

data Adverb = Adverb
  { adverbOnArray    :: Maybe (Array    -> St Function)
  , adverbOnFunction :: Maybe (Function -> St Function)
  , adverbRepr       :: String }

instance Show Adverb where
  show (Adverb _ _ repr) = repr

callOnArray :: Adverb -> Array -> St Function
callOnArray (Adverb (Just op) _ _) x = op x
callOnArray adv _ = throwError $ DomainError $ "Operator " ++ show adv ++ " does not take array operands."

callOnFunction :: Adverb -> Function -> St Function
callOnFunction (Adverb _ (Just op) _) x = op x
callOnFunction adv _ = throwError $ DomainError $ "Operator " ++ show adv ++ " does not take functions operands."

data Conjunction = Conjunction
  { conjOnArrayArray       :: Maybe (Array    -> Array    -> St Function)
  , conjOnArrayFunction    :: Maybe (Array    -> Function -> St Function)
  , conjOnFunctionArray    :: Maybe (Function -> Array    -> St Function)
  , conjOnFunctionFunction :: Maybe (Function -> Function -> St Function)
  , conjRepr               :: String }

instance Show Conjunction where
  show (Conjunction _ _ _ _ repr) = repr

callOnArrayAndArray :: Conjunction -> Array -> Array -> St Function
callOnArrayAndArray (Conjunction (Just op) _ _ _ _) x y = op x y
callOnArrayAndArray conj _ _ = throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to two arrays."

callOnArrayAndFunction :: Conjunction -> Array -> Function -> St Function
callOnArrayAndFunction (Conjunction _ (Just op) _ _ _) x y = op x y
callOnArrayAndFunction conj _ _ = throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to an array and a function."

callOnFunctionAndArray :: Conjunction -> Function -> Array -> St Function
callOnFunctionAndArray (Conjunction _ _ (Just op) _ _) x y = op x y
callOnFunctionAndArray conj _ _ = throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to a function and an array."

callOnFunctionAndFunction :: Conjunction -> Function -> Function -> St Function
callOnFunctionAndFunction (Conjunction _ _ _ (Just op) _) x y = op x y
callOnFunctionAndFunction conj _ _ = throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to two functions."

-- * Quads

data Nilad = Nilad
  { niladGet :: Maybe (St Array)
  , niladSet :: Maybe (Array -> St ())
  , niladRepr :: String }

instance Show Nilad where 
  show (Nilad { niladRepr = r }) = r

data Quads = Quads
  { quadArrays :: [(String, Nilad)]
  , quadFunctions :: [(String, Function)]
  , quadAdverbs :: [(String, Adverb)]
  , quadConjunctions :: [(String, Conjunction)] }
  deriving (Show)

-- * State

data Scope = Scope
  { scopeArrays :: [(String, Array)]
  , scopeFunctions :: [(String, Function)]
  , scopeAdverbs :: [(String, Adverb)]
  , scopeConjunctions :: [(String, Conjunction)]
  , scopeParent :: Maybe Scope }
  deriving (Show)

specialNames :: [String]
specialNames = [[G.alpha], [G.omega], [G.alpha, G.alpha], [G.omega, G.omega], [G.alphaBar, G.alphaBar], [G.omegaBar, G.omegaBar], [G.del], [G.underscore, G.del], [G.underscore, G.del, G.underscore]]

scopeLookupArray :: String -> Scope -> Maybe Array
scopeLookupArray name sc =
  lookup name (scopeArrays sc) <|> (if name `elem` specialNames then Nothing else scopeParent sc >>= scopeLookupArray name)

scopeLookupFunction :: String -> Scope -> Maybe Function
scopeLookupFunction name sc =
  lookup name (scopeFunctions sc) <|> (if name `elem` specialNames then Nothing else scopeParent sc >>= scopeLookupFunction name)

scopeLookupAdverb :: String -> Scope -> Maybe Adverb
scopeLookupAdverb name sc =
  lookup name (scopeAdverbs sc) <|> (if name `elem` specialNames then Nothing else scopeParent sc >>= scopeLookupAdverb name)

scopeLookupConjunction :: String -> Scope -> Maybe Conjunction
scopeLookupConjunction name sc =
  lookup name (scopeConjunctions sc) <|> (if name `elem` specialNames then Nothing else scopeParent sc >>= scopeLookupConjunction name)

scopeUpdateArray :: String -> Array -> Scope -> Scope
scopeUpdateArray name val sc = sc{ scopeArrays = update name val (scopeArrays sc) }

scopeUpdateFunction :: String -> Function -> Scope -> Scope
scopeUpdateFunction name val sc = sc{ scopeFunctions = update name val (scopeFunctions sc) }

scopeUpdateAdverb :: String -> Adverb -> Scope -> Scope
scopeUpdateAdverb name val sc = sc{ scopeAdverbs = update name val (scopeAdverbs sc) }

scopeUpdateConjunction :: String -> Conjunction -> Scope -> Scope
scopeUpdateConjunction name val sc = sc{ scopeConjunctions = update name val (scopeConjunctions sc) }

data Context = Context
  { contextScope :: Scope
  , contextQuads :: Quads
  , contextIn :: St String
  , contextOut :: String -> St ()
  , contextErr :: String -> St () }

type St = StateT Context (ExceptT Error IO)

runSt :: St a -> Context -> ResultIO (a, Context)
runSt = runStateT

liftToSt :: IO a -> St a
liftToSt = liftIO

putScope :: Scope -> St ()
putScope sc = do
  context <- get
  put $ context{ contextScope = sc }
