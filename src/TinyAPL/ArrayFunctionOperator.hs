{-# LANGUAGE FlexibleContexts, LambdaCase, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
module TinyAPL.ArrayFunctionOperator where

import TinyAPL.Error
import TinyAPL.Util
import qualified TinyAPL.Glyphs as G
import TinyAPL.Complex ( magnitude, realPart, Complex(..) )
import Numeric.Natural
import Data.List
import Control.Monad
import Data.Maybe (mapMaybe, fromJust)
import Control.Monad.State
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Data.List.NonEmpty (NonEmpty, toList)
import Data.Tuple (swap)
import qualified Data.Matrix as M
import qualified Data.IORef as IORef
import Data.IORef (IORef)
import Control.DeepSeq
import GHC.Generics

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
  | Wrap Function
  | AdverbWrap Adverb
  | ConjunctionWrap Conjunction
  | Struct Context
  deriving (Generic, NFData)

data Array = Array
  { arrayShape :: [Natural]
  , arrayContents :: [ScalarValue] }
  deriving (Generic, NFData)

-- * Array helper functions

arrayRank :: Array -> Natural
arrayRank (Array sh _) = genericLength sh

arrayDepth :: Array -> Natural
arrayDepth (Array [] [Box xs]) = 1 + arrayDepth xs
arrayDepth (Array [] _) = 0
arrayDepth (Array _ []) = 1
arrayDepth (Array _ xs) = 1 + maximum (arrayDepth . fromScalar <$> xs)

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

matrix :: M.Matrix ScalarValue -> Array
matrix mat = Array [toEnum $ M.nrows mat, toEnum $ M.ncols mat] $ M.toList mat

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
fromMajorCells (c:cs) = let
  impl = fromJust $ arrayReshaped (1 + genericLength cs : arrayShape c) $ concatMap arrayContents $ c : cs
  in if all ((== arrayShape c) . arrayShape) cs then impl else error "fromMajorCells: mismatched shapes"

-- * Number comparison functions

comparisonTolerance :: Double
comparisonTolerance = 1e-14

realEqual :: Double -> Double -> Bool
realEqual a b = if isInfinite a || isInfinite b then a == b else abs (a - b) <= comparisonTolerance * (abs a `max` abs b)
complexEqual :: Complex Double -> Complex Double -> Bool
complexEqual a@(ar :+ ai) b@(br :+ bi) =
  if isInfinite ar || isInfinite ai || isInfinite br || isInfinite bi
  then a == b
  else magnitude (a - b) <= comparisonTolerance * (magnitude a `max` magnitude b)

isReal :: Complex Double -> Bool
isReal (_ :+ b) = 0 `realEqual` b -- A number is real if its imaginary part compares equal to zero.

-- * Total ordering for scalars and arrays

instance Eq ScalarValue where
  (Character a) == (Character b) = a == b
  (Box as) == (Box bs) = as == bs
  (Number a) == (Number b)
    | isReal a && isReal b = realPart a `realEqual` realPart b
    | otherwise = a `complexEqual` b
  (Wrap a) == (Wrap b) = a == b
  (AdverbWrap a) == (AdverbWrap b) = a == b
  (ConjunctionWrap a) == (ConjunctionWrap b) = a == b
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
  (Box _) `compare` (Number _) = GT
  (Box _) `compare` (Character _) = GT
  (Box as) `compare` (Box bs) = as `compare` bs
  (Box _) `compare` _ = LT
  (Wrap _) `compare` (Number _) = GT
  (Wrap _) `compare` (Character _) = GT
  (Wrap _) `compare` (Box _) = GT
  (Wrap a) `compare` (Wrap b) = a `compare` b
  (Wrap _) `compare` _ = LT
  (AdverbWrap _) `compare` (Number _) = GT
  (AdverbWrap _) `compare` (Character _) = GT
  (AdverbWrap _) `compare` (Box _) = GT
  (AdverbWrap _) `compare` (Wrap _) = GT
  (AdverbWrap a) `compare` (AdverbWrap b) = a `compare` b
  (AdverbWrap _) `compare` _ = GT
  (ConjunctionWrap _) `compare` (Number _) = GT
  (ConjunctionWrap _) `compare` (Character _) = GT
  (ConjunctionWrap _) `compare` (Box _) = GT
  (ConjunctionWrap _) `compare` (Wrap _) = GT
  (ConjunctionWrap _) `compare` (AdverbWrap _) = GT
  (ConjunctionWrap a) `compare` (ConjunctionWrap b) = a `compare` b
  (ConjunctionWrap _) `compare` _ = GT
  (Struct _) `compare` (Number _) = GT
  (Struct _) `compare` (Character _) = GT
  (Struct _) `compare` (Box _) = GT
  (Struct _) `compare` (Wrap _) = GT
  (Struct _) `compare` (AdverbWrap _) = GT
  (Struct _) `compare` (ConjunctionWrap _) = GT
  (Struct _) `compare` (Struct _) = LT

instance Eq Array where
  -- Two arrays are equal iff both their shapes and their ravels are equal.
  (Array ash as) == (Array bsh bs) = (ash, as) == (bsh, bs)

instance Ord Array where
  -- Arrays are ordered by shape and then contents
  (Array ash as) `compare` (Array bsh bs) = (ash `compare` bsh) <> (as `compare` bs)

isInt :: Double -> Bool
isInt = realEqual <*> (fromInteger . round)

-- * @Show@ for scalars and arrays

showComplex :: Complex Double -> String
showComplex (a :+ b)
  | b `realEqual` 0 = showAplDouble a
  | otherwise = showAplDouble a ++ [G.imaginary] ++ showAplDouble b

instance Show ScalarValue where
  show (Number x) = showComplex x
  show (Character x) = [x]
  show (Box xs) = G.enclose : show xs
  show (Wrap fn) = [G.wrap, fst G.parens] ++ show fn ++ [snd G.parens]
  show (AdverbWrap adv) = [G.wrap, fst G.parens] ++ show adv ++ [snd G.parens]
  show (ConjunctionWrap conj) = [G.wrap, fst G.parens] ++ show conj ++ [snd G.parens]
  show (Struct _) = [fst G.struct] ++ "..." ++ [snd G.struct]

showElement :: ScalarValue -> String
showElement (Box xs) = show xs
showElement (Wrap fn) = show fn
showElement (AdverbWrap adv) = show adv
showElement (ConjunctionWrap conj) = show conj
showElement x = show x

instance Show Array where
  show (Array [] [s])                     = show s
  show (Array [_] xs)
    | not (null xs) && all isCharacter xs = xs >>= show
    | otherwise                           = [fst G.vector] ++ intercalate [' ', G.separator, ' '] (showElement <$> xs) ++ [snd G.vector]
  show arr                                = [fst G.highRank] ++ intercalate [' ', G.separator, ' '] (show <$> majorCells arr) ++ [snd G.highRank]

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
scalarRepr (Wrap fn) = [G.wrap, fst G.parens] ++ show fn ++ [snd G.parens]
scalarRepr (AdverbWrap adv) = [G.wrap, fst G.parens] ++ show adv ++ [snd G.parens]
scalarRepr (ConjunctionWrap conj) = [G.wrap, fst G.parens] ++ show conj ++ [snd G.parens]
scalarRepr (Struct _) = [fst G.struct] ++ "..." ++ [snd G.struct]

stringRepr :: [Char] -> String
stringRepr str = [G.stringDelimiter] ++ concatMap (fst . charRepr) str ++ [G.stringDelimiter]

arrayRepr :: Array -> String
arrayRepr (Array [] [s]) = scalarRepr s
arrayRepr (Array [_] xs)
  | not (null xs) && all isCharacter xs = stringRepr $ asCharacter' <$> xs
  | otherwise = [fst G.vector] ++ intercalate [' ', G.separator, ' '] (arrayRepr . fromScalar <$> xs) ++ [snd G.vector]
arrayRepr arr = [fst G.highRank] ++ intercalate [' ', G.separator, ' '] (arrayRepr <$> majorCells arr) ++ [snd G.highRank]

-- * Conversions

isNumber :: ScalarValue -> Bool
isNumber (Number _) = True
isNumber _ = False

isCharacter :: ScalarValue -> Bool
isCharacter (Character _) = True
isCharacter _ = False

boolToScalar :: Bool -> ScalarValue
boolToScalar True = Number 1
boolToScalar False = Number 0

asWrap :: MonadError Error m => Error -> ScalarValue -> m Function
asWrap _ (Wrap fn) = pure fn
asWrap e _ = throwError e

asAdverbWrap :: MonadError Error m => Error -> ScalarValue -> m Adverb
asAdverbWrap _ (AdverbWrap adv) = pure adv
asAdverbWrap e _ = throwError e

asConjunctionWrap :: MonadError Error m => Error -> ScalarValue -> m Conjunction
asConjunctionWrap _ (ConjunctionWrap conj) = pure conj
asConjunctionWrap e _ = throwError e

asStruct :: MonadError Error m => Error -> ScalarValue -> m Context
asStruct _ (Struct ctx) = pure ctx
asStruct e _ = throwError e

asBool :: MonadError Error m => Error -> ScalarValue -> m Bool
asBool _ (Number 0) = pure False
asBool _ (Number 1) = pure True
asBool e _ = throwError e

asNumber :: MonadError Error m => Error -> ScalarValue -> m (Complex Double)
asNumber _ (Number x) = pure x
asNumber e _ = throwError e

asCharacter :: MonadError Error m => Error -> ScalarValue -> m Char
asCharacter _ (Character x) = pure x
asCharacter e _ = throwError e

asCharacter' :: ScalarValue -> Char
asCharacter' (Character x) = x
asCharacter' _ = error "asCharacter': not a character"

asReal :: MonadError Error m => Error -> Complex Double -> m Double
asReal e x
  | isReal x = pure $ realPart x
  | otherwise = throwError e

-- These need to be somewhat large
likePositiveInfinity :: Integral num => num
likePositiveInfinity = fromInteger $ toInteger (maxBound `div` 2 :: Int)

likeNegativeInfinity :: Integral num => num
likeNegativeInfinity = fromInteger $ toInteger (minBound `div` 2 :: Int)

asInt' :: MonadError Error m => Integral num => Error -> Double -> m num
asInt' e x
  | isInfinite x && x > 0 = pure likePositiveInfinity
  | isInfinite x && x < 0 = pure likeNegativeInfinity
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

asString :: MonadError Error m => Error -> Array -> m String
asString err = asVector err >=> mapM (asCharacter err)

asStrings :: MonadError Error m => Error -> Array -> m [String]
asStrings _ (Array [] [Character x]) = pure [[x]]
asStrings _ (Array [_] vec) | all isCharacter vec = pure [asCharacter' <$> vec]
asStrings err (Array [_] vec) = mapM (asString err . fromScalar) vec
asStrings err _ = throwError err

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

asMatrix :: MonadError Error m => Error -> Array -> m (M.Matrix ScalarValue)
asMatrix _ (Array [] scalar)        = pure $ M.fromList 1 1 scalar
asMatrix _ (Array [cols] vec)       = pure $ M.fromList 1 (fromEnum cols) vec
asMatrix _ (Array [rows, cols] mat) = pure $ M.fromList (fromEnum rows) (fromEnum cols) mat
asMatrix e _                        = throwError e

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
  = DefinedFunction
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionRepr  :: String
    , functionContext :: Maybe Context
    , definedFunctionId :: Integer }
  | PrimitiveFunction
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionRepr  :: String
    , functionContext :: Maybe Context }
  | PartialFunction
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionContext :: Maybe Context
    , partialFunctionFunction :: Function
    , partialFunctionLeft :: Array }
  | DerivedFunctionArray
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionContext :: Maybe Context
    , derivedFunctionAdverb :: Adverb
    , derivedFunctionArrayLeft :: Array }
  | DerivedFunctionFunction
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionContext :: Maybe Context
    , derivedFunctionAdverb :: Adverb
    , derivedFunctionFunctionLeft :: Function }
  | DerivedFunctionArrayArray
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionArrayLeft :: Array
    , derivedFunctionArrayRight :: Array }
  | DerivedFunctionArrayFunction
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionArrayLeft :: Array
    , derivedFunctionFunctionRight :: Function }
  | DerivedFunctionFunctionArray
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionFunctionLeft :: Function
    , derivedFunctionArrayRight :: Array }
  | DerivedFunctionFunctionFunction
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionContext :: Maybe Context
    , derivedFunctionConjunction :: Conjunction
    , derivedFunctionFunctionLeft :: Function
    , derivedFunctionFunctionRight :: Function }
  | UnwrapArrayFunction
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionContext :: Maybe Context
    , unwrapFunctionArray :: Array }
  | TrainFunction
    { functionMonad :: Maybe (Array -> St Array)
    , functionDyad  :: Maybe (Array -> Array -> St Array)
    , functionContext :: Maybe Context
    , trainFunctionTines :: [Maybe Value] }
  deriving (Generic, NFData)

instance Eq Function where
  DefinedFunction { definedFunctionId = a } == DefinedFunction { definedFunctionId = b } = a == b
  PrimitiveFunction { functionRepr = a } == PrimitiveFunction { functionRepr = b } = a == b
  PartialFunction { partialFunctionFunction = af, partialFunctionLeft = al } == PartialFunction { partialFunctionFunction = bf, partialFunctionLeft = bl } = af == bf && al == bl
  DerivedFunctionArray { derivedFunctionAdverb = aadv, derivedFunctionArrayLeft = aa } == DerivedFunctionArray { derivedFunctionAdverb = badv, derivedFunctionArrayLeft = ba } = aadv == badv && aa == ba
  DerivedFunctionFunction { derivedFunctionAdverb = aadv, derivedFunctionFunctionLeft = aa } == DerivedFunctionFunction { derivedFunctionAdverb = badv, derivedFunctionFunctionLeft = ba } = aadv == badv && aa == ba
  DerivedFunctionArrayArray { derivedFunctionConjunction = aconj, derivedFunctionArrayLeft = aa, derivedFunctionArrayRight = ab } == DerivedFunctionArrayArray { derivedFunctionConjunction = bconj, derivedFunctionArrayLeft = ba, derivedFunctionArrayRight = bb } = aconj == bconj && aa == ba && ab == bb
  DerivedFunctionArrayFunction { derivedFunctionConjunction = aconj, derivedFunctionArrayLeft = aa, derivedFunctionFunctionRight = ab } == DerivedFunctionArrayFunction { derivedFunctionConjunction = bconj, derivedFunctionArrayLeft = ba, derivedFunctionFunctionRight = bb } = aconj == bconj && aa == ba && ab == bb
  DerivedFunctionFunctionArray { derivedFunctionConjunction = aconj, derivedFunctionFunctionLeft = aa, derivedFunctionArrayRight = ab } == DerivedFunctionFunctionArray { derivedFunctionConjunction = bconj, derivedFunctionFunctionLeft = ba, derivedFunctionArrayRight = bb } = aconj == bconj && aa == ba && ab == bb
  DerivedFunctionFunctionFunction { derivedFunctionConjunction = aconj, derivedFunctionFunctionLeft = aa, derivedFunctionFunctionRight = ab } == DerivedFunctionFunctionFunction { derivedFunctionConjunction = bconj, derivedFunctionFunctionLeft = ba, derivedFunctionFunctionRight = bb } = aconj == bconj && aa == ba && ab == bb
  UnwrapArrayFunction { unwrapFunctionArray = a } == UnwrapArrayFunction { unwrapFunctionArray = b } = a == b
  TrainFunction { trainFunctionTines = a } == TrainFunction { trainFunctionTines = b } = a == b
  _ == _ = False

instance Ord Function where
  DefinedFunction { definedFunctionId = a } `compare` DefinedFunction { definedFunctionId = b } = a `compare` b
  DefinedFunction {} `compare` _ = LT
  PrimitiveFunction {} `compare` DefinedFunction {} = GT
  PrimitiveFunction { functionRepr = a } `compare` PrimitiveFunction { functionRepr = b } = a `compare` b
  PrimitiveFunction {} `compare` _ = LT
  PartialFunction {} `compare` DefinedFunction {} = GT
  PartialFunction {} `compare` PrimitiveFunction {} = GT
  PartialFunction { partialFunctionLeft = a } `compare` PartialFunction { partialFunctionLeft = b } = a `compare` b
  PartialFunction {} `compare` _ = LT
  DerivedFunctionArray {} `compare` DefinedFunction {} = GT
  DerivedFunctionArray {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionArray {} `compare` PartialFunction {} = GT
  DerivedFunctionArray { derivedFunctionAdverb = aadv, derivedFunctionArrayLeft = aa } `compare` DerivedFunctionArray { derivedFunctionAdverb = badv, derivedFunctionArrayLeft = ba } = aadv `compare` badv <> aa `compare` ba
  DerivedFunctionArray {} `compare` _ = LT
  DerivedFunctionFunction {} `compare` DefinedFunction {} = GT
  DerivedFunctionFunction {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionFunction {} `compare` PartialFunction {} = GT
  DerivedFunctionFunction {} `compare` DerivedFunctionArray {} = GT
  DerivedFunctionFunction { derivedFunctionAdverb = aadv, derivedFunctionFunctionLeft = aa } `compare` DerivedFunctionFunction { derivedFunctionAdverb = badv, derivedFunctionFunctionLeft = ba } = aadv `compare` badv <> aa `compare` ba
  DerivedFunctionFunction {} `compare` _ = LT
  DerivedFunctionArrayArray {} `compare` DefinedFunction {} = GT
  DerivedFunctionArrayArray {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionArrayArray {} `compare` PartialFunction {} = GT
  DerivedFunctionArrayArray {} `compare` DerivedFunctionArray {} = GT
  DerivedFunctionArrayArray {} `compare` DerivedFunctionFunction {} = GT
  DerivedFunctionArrayArray { derivedFunctionConjunction = aconj, derivedFunctionArrayLeft = aa, derivedFunctionArrayRight = ab }
    `compare` DerivedFunctionArrayArray { derivedFunctionConjunction = bconj, derivedFunctionArrayLeft = ba, derivedFunctionArrayRight = bb } = aconj `compare` bconj <> aa `compare` ba <> ab `compare` bb
  DerivedFunctionArrayArray {} `compare` _ = LT
  DerivedFunctionArrayFunction {} `compare` DefinedFunction {} = GT
  DerivedFunctionArrayFunction {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionArrayFunction {} `compare` PartialFunction {} = GT
  DerivedFunctionArrayFunction {} `compare` DerivedFunctionArray {} = GT
  DerivedFunctionArrayFunction {} `compare` DerivedFunctionFunction {} = GT
  DerivedFunctionArrayFunction {} `compare` DerivedFunctionArrayArray {} = GT
  DerivedFunctionArrayFunction { derivedFunctionConjunction = aconj, derivedFunctionArrayLeft = aa, derivedFunctionFunctionRight = ab }
    `compare` DerivedFunctionArrayFunction { derivedFunctionConjunction = bconj, derivedFunctionArrayLeft = ba, derivedFunctionFunctionRight = bb } = aconj `compare` bconj <> aa `compare` ba <> ab `compare` bb
  DerivedFunctionArrayFunction {} `compare` _ = LT
  DerivedFunctionFunctionArray {} `compare` DefinedFunction {} = GT
  DerivedFunctionFunctionArray {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionFunctionArray {} `compare` PartialFunction {} = GT
  DerivedFunctionFunctionArray {} `compare` DerivedFunctionArray {} = GT
  DerivedFunctionFunctionArray {} `compare` DerivedFunctionFunction {} = GT
  DerivedFunctionFunctionArray {} `compare` DerivedFunctionArrayArray {} = GT
  DerivedFunctionFunctionArray {} `compare` DerivedFunctionArrayFunction {} = GT
  DerivedFunctionFunctionArray { derivedFunctionConjunction = aconj, derivedFunctionFunctionLeft = aa, derivedFunctionArrayRight = ab }
    `compare` DerivedFunctionFunctionArray { derivedFunctionConjunction = bconj, derivedFunctionFunctionLeft = ba, derivedFunctionArrayRight = bb } = aconj `compare` bconj <> aa `compare` ba <> ab `compare` bb
  DerivedFunctionFunctionArray {} `compare` _ = LT
  DerivedFunctionFunctionFunction {} `compare` DefinedFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` PrimitiveFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` PartialFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionArray {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionArrayArray {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionArrayFunction {} = GT
  DerivedFunctionFunctionFunction {} `compare` DerivedFunctionFunctionArray {} = GT
  DerivedFunctionFunctionFunction { derivedFunctionConjunction = aconj, derivedFunctionFunctionLeft = aa, derivedFunctionFunctionRight = ab }
    `compare` DerivedFunctionFunctionFunction { derivedFunctionConjunction = bconj, derivedFunctionFunctionLeft = ba, derivedFunctionFunctionRight = bb } = aconj `compare` bconj <> aa `compare` ba <> ab `compare` bb
  DerivedFunctionFunctionFunction {} `compare` _ = LT
  UnwrapArrayFunction {} `compare` DefinedFunction {} = GT
  UnwrapArrayFunction {} `compare` PrimitiveFunction {} = GT
  UnwrapArrayFunction {} `compare` PartialFunction {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionArray {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionFunction {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionArrayArray {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionArrayFunction {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionFunctionArray {} = GT
  UnwrapArrayFunction {} `compare` DerivedFunctionFunctionFunction {} = GT
  UnwrapArrayFunction { unwrapFunctionArray = a } `compare` UnwrapArrayFunction { unwrapFunctionArray = b } = a `compare` b
  UnwrapArrayFunction {} `compare` _ = LT
  TrainFunction {} `compare` DefinedFunction {} = GT
  TrainFunction {} `compare` PrimitiveFunction {} = GT
  TrainFunction {} `compare` PartialFunction {} = GT
  TrainFunction {} `compare` DerivedFunctionArray {} = GT
  TrainFunction {} `compare` DerivedFunctionFunction {} = GT
  TrainFunction {} `compare` DerivedFunctionArrayArray {} = GT
  TrainFunction {} `compare` DerivedFunctionArrayFunction {} = GT
  TrainFunction {} `compare` DerivedFunctionFunctionArray {} = GT
  TrainFunction {} `compare` DerivedFunctionFunctionFunction {} = GT
  TrainFunction {} `compare` UnwrapArrayFunction {} = GT
  TrainFunction { trainFunctionTines = a } `compare` TrainFunction { trainFunctionTines = b } = a `compare` b

showTine :: Maybe Value -> String
showTine Nothing = ""
showTine (Just x) = show x

instance Show Function where
  show (DefinedFunction { functionRepr = repr }) = repr
  show (PrimitiveFunction { functionRepr = repr }) = repr
  show (PartialFunction { partialFunctionFunction = fn, partialFunctionLeft = n }) = [fst G.parens] ++ show n ++ [snd G.parens] ++ show fn
  show (DerivedFunctionArray { derivedFunctionAdverb = adv, derivedFunctionArrayLeft = n }) = [fst G.parens] ++ show n ++ [snd G.parens] ++ show adv
  show (DerivedFunctionFunction { derivedFunctionAdverb = adv, derivedFunctionFunctionLeft = u }) = [fst G.parens] ++ show u ++ [snd G.parens] ++ show adv
  show (DerivedFunctionArrayArray { derivedFunctionConjunction = conj, derivedFunctionArrayLeft = n, derivedFunctionArrayRight = m }) = [fst G.parens] ++ show n ++ [snd G.parens] ++ show conj ++ [fst G.parens] ++ show m ++ [snd G.parens]
  show (DerivedFunctionArrayFunction { derivedFunctionConjunction = conj, derivedFunctionArrayLeft = n, derivedFunctionFunctionRight = v }) = [fst G.parens] ++ show n ++ [snd G.parens] ++ show conj ++ [fst G.parens] ++ show v ++ [snd G.parens]
  show (DerivedFunctionFunctionArray { derivedFunctionConjunction = conj, derivedFunctionFunctionLeft = u, derivedFunctionArrayRight = m }) = [fst G.parens] ++ show u ++ [snd G.parens] ++ show conj ++ [fst G.parens] ++ show m ++ [snd G.parens]
  show (DerivedFunctionFunctionFunction { derivedFunctionConjunction = conj, derivedFunctionFunctionLeft = u, derivedFunctionFunctionRight = v }) = [fst G.parens] ++ show u ++ [snd G.parens] ++ show conj ++ [fst G.parens] ++ show v ++ [snd G.parens]
  show (UnwrapArrayFunction { unwrapFunctionArray = arr }) = [G.unwrap, fst G.parens] ++ show arr ++ [snd G.parens]
  show (TrainFunction { trainFunctionTines = tines }) = [fst G.train] ++ intercalate [' ', G.separator, ' '] (showTine <$> tines) ++ [snd G.train]

noMonad :: String -> Error
noMonad str = DomainError $ "Function " ++ str ++ " cannot be called monadically"

callMonad :: Function -> Array -> St Array
callMonad f x = case functionMonad f of
  Just m -> case functionContext f of
    Just ctx -> runWithContext ctx $ m x
    Nothing -> m x
  Nothing -> throwError $ noMonad $ show f

noDyad :: String -> Error
noDyad str = DomainError $ "Function " ++ str ++ " cannot be called dyadically"

callDyad :: Function -> Array -> Array -> St Array
callDyad f a b = case functionDyad f of
  Just d -> case functionContext f of
    Just ctx -> runWithContext ctx $ d a b
    Nothing -> d a b
  Nothing -> throwError $ noDyad $ show f

-- * Operators

data Adverb
  = DefinedAdverb
    { adverbOnArray            :: Maybe (Array    -> St Function)
    , adverbOnFunction         :: Maybe (Function -> St Function)
    , adverbRepr               :: String
    , adverbContext            :: Maybe Context
    , definedAdverbId          :: Integer }
  | PrimitiveAdverb
    { adverbOnArray            :: Maybe (Array    -> St Function)
    , adverbOnFunction         :: Maybe (Function -> St Function)
    , adverbRepr               :: String
    , adverbContext            :: Maybe Context }
  | PartialAdverb
    { adverbOnArray            :: Maybe (Array    -> St Function)
    , adverbOnFunction         :: Maybe (Function -> St Function)
    , adverbContext            :: Maybe Context
    , partialAdverbConjunction :: Conjunction
    , partialAdverbRight       :: Value }
  | TrainAdverb
    { adverbOnArray            :: Maybe (Array    -> St Function)
    , adverbOnFunction         :: Maybe (Function -> St Function)
    , adverbContext            :: Maybe Context
    , trainAdverbTines         :: [Maybe Value] }
  deriving (Generic, NFData)

instance Show Adverb where
  show DefinedAdverb { adverbRepr = repr } = repr
  show PrimitiveAdverb { adverbRepr = repr } = repr
  show PartialAdverb { partialAdverbConjunction = conj, partialAdverbRight = n } = show conj ++ [fst G.parens] ++ show n ++ [snd G.parens]
  show TrainAdverb { trainAdverbTines = tines } = [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] (showTine <$> tines) ++ [snd G.train]

instance Eq Adverb where
  DefinedAdverb { definedAdverbId = a } == DefinedAdverb { definedAdverbId = b } = a == b
  PrimitiveAdverb { adverbRepr = a } == PrimitiveAdverb { adverbRepr = b } = a == b
  PartialAdverb { partialAdverbConjunction = ac, partialAdverbRight = ar } == PartialAdverb { partialAdverbConjunction = bc, partialAdverbRight = br } = ac == bc && ar == br
  TrainAdverb { trainAdverbTines = a } == TrainAdverb { trainAdverbTines = b } = a == b
  _ == _ = False

instance Ord Adverb where
  DefinedAdverb { definedAdverbId = a } `compare` DefinedAdverb { definedAdverbId = b } = a `compare` b
  DefinedAdverb {} `compare` _ = LT
  PrimitiveAdverb {} `compare` DefinedAdverb {} = GT
  PrimitiveAdverb { adverbRepr = a } `compare` PrimitiveAdverb { adverbRepr = b } = a `compare` b
  PrimitiveAdverb {} `compare` _ = LT
  PartialAdverb {} `compare` DefinedAdverb {} = GT
  PartialAdverb {} `compare` PrimitiveAdverb {} = GT
  PartialAdverb { partialAdverbConjunction = ac, partialAdverbRight = ar } `compare` PartialAdverb { partialAdverbConjunction = bc, partialAdverbRight = br } = ac `compare` bc <> ar `compare` br
  PartialAdverb {} `compare` _ = LT
  TrainAdverb {} `compare` DefinedAdverb {} = GT
  TrainAdverb {} `compare` PrimitiveAdverb {} = GT
  TrainAdverb {} `compare` PartialAdverb {} = GT
  TrainAdverb { trainAdverbTines = a } `compare` TrainAdverb { trainAdverbTines = b } = a `compare` b

callOnArray :: Adverb -> Array -> St Function
callOnArray adv x = case adverbOnArray adv of
  Just f -> case adverbContext adv of
    Just ctx -> runWithContext ctx $ f x
    Nothing -> f x
  Nothing -> throwError $ DomainError $ "Operator " ++ show adv ++ " does not take array operands."

callOnFunction :: Adverb -> Function -> St Function
callOnFunction adv x = case adverbOnFunction adv of
  Just f -> case adverbContext adv of
    Just ctx -> runWithContext ctx $ f x
    Nothing -> f x
  Nothing -> throwError $ DomainError $ "Operator " ++ show adv ++ " does not take functions operands."

data Conjunction
  = DefinedConjunction
    { conjOnArrayArray       :: Maybe (Array    -> Array    -> St Function)
    , conjOnArrayFunction    :: Maybe (Array    -> Function -> St Function)
    , conjOnFunctionArray    :: Maybe (Function -> Array    -> St Function)
    , conjOnFunctionFunction :: Maybe (Function -> Function -> St Function)
    , conjRepr               :: String
    , conjContext            :: Maybe Context
    , definedConjunctionId   :: Integer }
  | PrimitiveConjunction
    { conjOnArrayArray       :: Maybe (Array    -> Array    -> St Function)
    , conjOnArrayFunction    :: Maybe (Array    -> Function -> St Function)
    , conjOnFunctionArray    :: Maybe (Function -> Array    -> St Function)
    , conjOnFunctionFunction :: Maybe (Function -> Function -> St Function)
    , conjRepr               :: String
    , conjContext            :: Maybe Context }
  | TrainConjunction
    { conjOnArrayArray       :: Maybe (Array    -> Array    -> St Function)
    , conjOnArrayFunction    :: Maybe (Array    -> Function -> St Function)
    , conjOnFunctionArray    :: Maybe (Function -> Array    -> St Function)
    , conjOnFunctionFunction :: Maybe (Function -> Function -> St Function)
    , conjContext            :: Maybe Context
    , trainConjunctionTines  :: [Maybe Value] }
  deriving (Generic, NFData)
  
instance Show Conjunction where
  show DefinedConjunction { conjRepr = repr } = repr
  show PrimitiveConjunction { conjRepr = repr } = repr
  show TrainConjunction { trainConjunctionTines = tines } = [G.underscore, fst G.train] ++ intercalate [' ', G.separator, ' '] (showTine <$> tines) ++ [snd G.train, G.underscore]

instance Eq Conjunction where
  DefinedConjunction { definedConjunctionId = a } == DefinedConjunction { definedConjunctionId = b } = a == b
  PrimitiveConjunction { conjRepr = a } == PrimitiveConjunction { conjRepr = b } = a == b
  TrainConjunction { trainConjunctionTines = a } == TrainConjunction { trainConjunctionTines = b } = a == b
  _ == _ = False

instance Ord Conjunction where
  compare :: Conjunction -> Conjunction -> Ordering
  DefinedConjunction { definedConjunctionId = a } `compare` DefinedConjunction { definedConjunctionId = b } = a `compare` b
  DefinedConjunction {} `compare` _ = LT
  PrimitiveConjunction {} `compare` DefinedConjunction {} = GT
  PrimitiveConjunction { conjRepr = a } `compare` PrimitiveConjunction { conjRepr = b } = a `compare` b
  PrimitiveConjunction {} `compare` _ = LT
  TrainConjunction {} `compare` DefinedConjunction {} = GT
  TrainConjunction {} `compare` PrimitiveConjunction {} = GT
  TrainConjunction { trainConjunctionTines = a } `compare` TrainConjunction { trainConjunctionTines = b } = a `compare` b

callOnArrayAndArray :: Conjunction -> Array -> Array -> St Function
callOnArrayAndArray conj x y = case conjOnArrayArray conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f x y
    Nothing -> f x y
  Nothing -> throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to two arrays."

callOnArrayAndFunction :: Conjunction -> Array -> Function -> St Function
callOnArrayAndFunction conj x y = case conjOnArrayFunction conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f x y
    Nothing -> f x y
  Nothing -> throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to an array and a function."

callOnFunctionAndArray :: Conjunction -> Function -> Array -> St Function
callOnFunctionAndArray conj x y = case conjOnFunctionArray conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f x y
    Nothing -> f x y
  Nothing -> throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to a function and an array."

callOnFunctionAndFunction :: Conjunction -> Function -> Function -> St Function
callOnFunctionAndFunction conj x y = case conjOnFunctionFunction conj of
  Just f -> case conjContext conj of
    Just ctx -> runWithContext ctx $ f x y
    Nothing -> f x y
  Nothing -> throwError $ DomainError $ "Operator " ++ show conj ++ " cannot be applied to two functions."

-- * Quads

data Nilad = Nilad
  { niladGet :: Maybe (St Array)
  , niladSet :: Maybe (Array -> St ())
  , niladRepr :: String
  , niladContext :: Maybe Context }

instance NFData Nilad where
  rnf (Nilad g s r c) = rwhnf g `seq` rwhnf s `seq` rnf r `seq` rnf c `seq` ()

getNilad :: Nilad -> St Array
getNilad (Nilad (Just g) _ _ (Just ctx)) = runWithContext ctx g
getNilad (Nilad (Just g) _ _ Nothing) = g
getNilad g@(Nilad Nothing _ _ _) = throwError $ DomainError $ "Nilad " ++ show g ++ " cannot be get"

setNilad :: Nilad -> Array -> St ()
setNilad (Nilad _ (Just s) _ (Just ctx)) x = runWithContext ctx $ s x
setNilad (Nilad _ (Just s) _ Nothing) x = s x
setNilad s@(Nilad _ Nothing _ _) _ = throwError $ DomainError $ "Nilad " ++ show s ++ " cannot be set"

instance Show Nilad where 
  show (Nilad { niladRepr = r }) = r

data Quads = Quads
  { quadArrays :: [(String, Nilad)]
  , quadFunctions :: [(String, Function)]
  , quadAdverbs :: [(String, Adverb)]
  , quadConjunctions :: [(String, Conjunction)] }
  deriving (Show, Generic, NFData)

instance Semigroup Quads where
  (Quads aAr aFn aAd aCn) <> (Quads bAr bFn bAd bCn) = Quads (aAr ++ bAr) (aFn ++ bFn) (aAd ++ bAd) (aCn ++ bCn)

instance Monoid Quads where
  mempty = Quads [] [] [] []
  
quadsFromReprs :: [Nilad] -> [Function] -> [Adverb] -> [Conjunction] -> Quads
quadsFromReprs ns fs as cs = Quads ((\x -> (niladRepr x, x)) <$> ns) ((\x -> (functionRepr x, x)) <$> fs) ((\x -> (adverbRepr x, x)) <$> as) ((\x -> (conjRepr x, x)) <$> cs)

-- * State

data VariableType
  = VariableNormal
  | VariableConstant
  | VariablePrivate
  deriving (Show, Eq, Generic, NFData)

data Scope = Scope
  { scopeArrays :: [(String, (VariableType, Array))]
  , scopeFunctions :: [(String, (VariableType, Function))]
  , scopeAdverbs :: [(String, (VariableType, Adverb))]
  , scopeConjunctions :: [(String, (VariableType, Conjunction))]
  , scopeParent :: Maybe (IORef Scope) }
  deriving (Generic, NFData)

instance Show Scope where
  show (Scope arr fn adv conj p) = "Scope { arrays = " ++ show arr ++ ", functions = " ++ show fn ++ ", adverbs = " ++ show adv ++ ", conjunctions = " ++ show conj ++ ", " ++ (case p of
    Nothing -> "no parent"
    Just _ -> "a parent") ++ " }"

specialNames :: [String]
specialNames = [[G.alpha], [G.omega], [G.alpha, G.alpha], [G.omega, G.omega], [G.alphaBar, G.alphaBar], [G.omegaBar, G.omegaBar], [G.del], [G.underscore, G.del], [G.underscore, G.del, G.underscore]]

scopeShallowLookupArray :: Bool -> String -> Scope -> Maybe Array
scopeShallowLookupArray private name sc = case lookup name (scopeArrays sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeShallowLookupFunction :: Bool -> String -> Scope -> Maybe Function
scopeShallowLookupFunction private name sc = case lookup name (scopeFunctions sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeShallowLookupAdverb :: Bool -> String -> Scope -> Maybe Adverb
scopeShallowLookupAdverb private name sc = case lookup name (scopeAdverbs sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeShallowLookupConjunction :: Bool -> String -> Scope -> Maybe Conjunction
scopeShallowLookupConjunction private name sc = case lookup name (scopeConjunctions sc) of
  Nothing -> Nothing
  Just (VariableNormal, x) -> Just x
  Just (VariableConstant, x) -> Just x
  Just (VariablePrivate, x) | private -> Just x
  Just (VariablePrivate, _) -> Nothing

scopeLookupArray :: Bool ->  String -> Scope -> St (Maybe Array)
scopeLookupArray private name sc = case scopeShallowLookupArray private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure Nothing else case scopeParent sc of
    Nothing -> pure Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupArray private name

scopeLookupFunction :: Bool -> String -> Scope -> St (Maybe Function)
scopeLookupFunction private name sc = case scopeShallowLookupFunction private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure $ Nothing else case scopeParent sc of
    Nothing -> pure $ Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupFunction private name

scopeLookupAdverb :: Bool -> String -> Scope -> St (Maybe Adverb)
scopeLookupAdverb private name sc = case scopeShallowLookupAdverb private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure $ Nothing else case scopeParent sc of
    Nothing -> pure $ Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupAdverb private name

scopeLookupConjunction :: Bool -> String -> Scope -> St (Maybe Conjunction)
scopeLookupConjunction private name sc = case scopeShallowLookupConjunction private name sc of
  Just x -> pure $ Just x
  Nothing -> if name `elem` specialNames then pure $ Nothing else case scopeParent sc of
    Nothing -> pure $ Nothing
    Just p -> (liftToSt $ IORef.readIORef p) >>= scopeLookupConjunction private name

scopeUpdateArray :: Bool ->  String -> VariableType -> Array -> Scope -> St Scope
scopeUpdateArray private name ty val sc = case lookup name (scopeArrays sc) of
  Nothing -> pure $ sc{ scopeArrays = update name (ty, val) (scopeArrays sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeArrays = update name (VariableNormal, val) (scopeArrays sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeArrays = update name (VariablePrivate, val) (scopeArrays sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeUpdateFunction :: Bool -> String -> VariableType -> Function -> Scope -> St Scope
scopeUpdateFunction private name ty val sc = case lookup name (scopeFunctions sc) of
  Nothing -> pure $ sc{ scopeFunctions = update name (ty, val) (scopeFunctions sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeFunctions = update name (VariableNormal, val) (scopeFunctions sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeFunctions = update name (VariablePrivate, val) (scopeFunctions sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeUpdateAdverb :: Bool -> String -> VariableType -> Adverb -> Scope -> St Scope
scopeUpdateAdverb private name ty val sc = case lookup name (scopeAdverbs sc) of
  Nothing -> pure $ sc{ scopeAdverbs = update name (ty, val) (scopeAdverbs sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeAdverbs = update name (VariableNormal, val) (scopeAdverbs sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeAdverbs = update name (VariablePrivate, val) (scopeAdverbs sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeUpdateConjunction :: Bool -> String -> VariableType -> Conjunction -> Scope -> St Scope
scopeUpdateConjunction private name ty val sc = case lookup name (scopeConjunctions sc) of
  Nothing -> pure $ sc{ scopeConjunctions = update name (ty, val) (scopeConjunctions sc) }
  Just (VariableNormal, _) -> pure $ sc{ scopeConjunctions = update name (VariableNormal, val) (scopeConjunctions sc) }
  Just (VariablePrivate, _) | private -> pure $ sc{ scopeConjunctions = update name (VariablePrivate, val) (scopeConjunctions sc) }
  Just (VariablePrivate, _) -> throwError $ DomainError "Cannot access private variable"
  Just (VariableConstant, _) -> throwError $ DomainError "Cannot modify constant variable"

scopeModifyArray :: Bool -> String -> Array -> Scope -> St Scope
scopeModifyArray private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeArrays sc) of
  Just (t, _) -> scopeUpdateArray private name t val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyArray private name val >>= writeRef p >>= const (pure sc)

scopeModifyFunction :: Bool -> String -> Function -> Scope -> St Scope
scopeModifyFunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeFunctions sc) of
  Just _ -> scopeUpdateFunction private name VariableNormal val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyFunction private name val >>= writeRef p >>= const (pure sc)

scopeModifyAdverb :: Bool -> String -> Adverb -> Scope -> St Scope
scopeModifyAdverb private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeAdverbs sc) of
  Just _ -> scopeUpdateAdverb private name VariableNormal val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyAdverb private name val >>= writeRef p >>= const (pure sc)

scopeModifyConjunction :: Bool -> String -> Conjunction -> Scope -> St Scope
scopeModifyConjunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeConjunctions sc) of
  Just _ -> scopeUpdateConjunction private name VariableNormal val sc
  Nothing -> case scopeParent sc of
    Nothing -> throwError $ DomainError "Modifying a non-existent variable"
    Just p -> readRef p >>= scopeModifyConjunction private name val >>= writeRef p >>= const (pure sc)

scopeShallowModifyArray :: Bool -> String -> Array -> Scope -> St Scope
scopeShallowModifyArray private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeArrays sc) of
  Just _ -> scopeUpdateArray private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

scopeShallowModifyFunction :: Bool -> String -> Function -> Scope -> St Scope
scopeShallowModifyFunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeFunctions sc) of
  Just _ -> scopeUpdateFunction private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

scopeShallowModifyAdverb :: Bool -> String -> Adverb -> Scope -> St Scope
scopeShallowModifyAdverb private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeAdverbs sc) of
  Just _ -> scopeUpdateAdverb private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

scopeShallowModifyConjunction :: Bool -> String -> Conjunction -> Scope -> St Scope
scopeShallowModifyConjunction private name val sc = if name `elem` specialNames then throwError $ DomainError "Cannot modify special variable" else case lookup name (scopeConjunctions sc) of
  Just _ -> scopeUpdateConjunction private name VariableNormal val sc
  Nothing -> throwError $ DomainError "Modifying a non-existent variable"

data Context = Context
  { contextScope :: IORef Scope
  , contextQuads :: Quads
  , contextIn :: St String
  , contextOut :: String -> St ()
  , contextErr :: String -> St ()
  , contextIncrementalId :: IORef Integer }

assignId :: St Integer
assignId = do
  idRef <- gets contextIncrementalId
  id <- readRef idRef
  writeRef idRef $ id + 1
  pure id

instance NFData Context where
  rnf (Context s q i o e d) = rnf s `seq` rnf q `seq` rwhnf i `seq` rnf o `seq` rnf e `seq` rnf d `seq` ()

type St = StateT Context (ExceptT Error IO)

runSt :: St a -> Context -> ResultIO (a, Context)
runSt = runStateT

runWithContext :: Context -> St a -> St a
runWithContext ctx f = do
  r <- liftToSt $ runExceptT $ runSt f ctx
  case r of
    Left e -> throwError e
    Right (x, _) -> pure x

liftToSt :: IO a -> St a
liftToSt = liftIO

getContext :: St Context
getContext = get

getsContext :: (Context -> a) -> St a
getsContext = gets

putContext :: Context -> St ()
putContext = put

putScope :: IORef Scope -> St ()
putScope sc = do
  context <- get
  put $ context{ contextScope = sc }

createRef :: a -> St (IORef a)
createRef = liftToSt . IORef.newIORef

readRef :: IORef a -> St a
readRef = liftToSt . IORef.readIORef

writeRef :: IORef a -> a -> St ()
writeRef = liftToSt .: IORef.writeIORef

modifyRef :: IORef a -> (a -> a) -> St ()
modifyRef = liftToSt .: IORef.modifyIORef

-- * Value

data Value
  = VArray Array
  | VFunction Function
  | VAdverb Adverb
  | VConjunction Conjunction
  deriving (Eq, Ord, Generic, NFData)

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
