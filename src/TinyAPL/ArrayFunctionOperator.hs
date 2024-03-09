{-# LANGUAGE LambdaCase #-}
module TinyAPL.ArrayFunctionOperator where

import TinyAPL.Error
import TinyAPL.Util
import qualified TinyAPL.Glyphs as G
import Data.Complex ( magnitude, realPart, Complex(..) )
import Numeric.Natural
import Data.List
import Control.Monad
import Data.Maybe (catMaybes, mapMaybe)
import Control.Monad.State
import Control.Monad.Except
import Control.Applicative (Alternative((<|>)))

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

box :: Array -> ScalarValue
box b@(Array [] [Box _]) = Box b
box (Array [] [x]) = x
box arr = Box arr

scalar :: ScalarValue -> Array
scalar x = Array [] [x]

vector :: [ScalarValue] -> Array
vector xs = Array [genericLength xs] xs

arrayOf :: [Natural] -> [ScalarValue] -> Maybe Array
arrayOf sh cs
  | product sh == genericLength cs = Just $ Array sh cs
  | otherwise = Nothing

arrayReshaped :: [Natural] -> [ScalarValue] -> Array
arrayReshaped sh cs = Array sh $ genericTake (product sh) $ cycle cs

majorCells :: Array -> [Array]
majorCells a@(Array [] _) = [a]
majorCells (Array (_:sh) cs) = mapMaybe (arrayOf sh) $ chunk (product sh) cs where
  chunk _ [] = []
  chunk l xs = genericTake l xs : chunk l (genericDrop l xs)

fromMajorCells :: [Array] -> Array
fromMajorCells [] = Array [0] []
fromMajorCells (c:cs) = arrayReshaped (1 + genericLength cs : arrayShape c) $ concatMap arrayContents $ c : cs

-- * Number comparison functions

comparisonTolerance = 1e-14

realEqual a b = abs (a - b) <= comparisonTolerance * (abs a `max` abs b)
complexEqual a b = magnitude (a - b) <= comparisonTolerance * (magnitude a `max` magnitude b)

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

showComplex (a :+ b)
  | b `realEqual` 0 = showAplDouble a
  | otherwise = showAplDouble a ++ [G.imaginary] ++ showAplDouble b

instance Show ScalarValue where
  show (Number x) = showComplex x
  show (Character x) = [x]
  show (Box xs) = "[box " ++ show xs ++ "]"

-- We'll implement proper array formatting later.
instance Show Array where
  show (Array sh cs) =
    "{ array with " ++ [G.rho] ++ " = " ++ unwords (map show sh) ++
    " and " ++ [G.ravel] ++ " = " ++ show cs ++ " }"

-- * Conversions

boolToScalar True = Number 1
boolToScalar False = Number 0

asBool :: Error -> ScalarValue -> Result Bool
asBool _ (Number 0) = pure False
asBool _ (Number 1) = pure True
asBool e _ = err e

asNumber :: Error -> ScalarValue -> Result (Complex Double)
asNumber _ (Number x) = pure x
asNumber e _ = err e

asReal :: Error -> Complex Double -> Result Double
asReal e x
  | isReal x = pure $ realPart x
  | otherwise = err e

asInt' :: Integral num => Error -> Double -> Result num
asInt' e x
  | isInt x = pure $ fromInteger $ floor x
  | otherwise = err e

asInt :: Integral num => Error -> Complex Double -> Result num
asInt e = asInt' e <=< asReal e

asNat' :: Integral num => Error -> num -> Result Natural
asNat' e x
  | x >= 0 = pure $ toEnum $ fromEnum x
  | otherwise = err e

asNat :: Error -> Complex Double -> Result Natural
asNat e = asNat' e <=< asInt e

isScalar :: Array -> Bool
isScalar (Array [] _) = True
isScalar _ = False

asScalar :: Error -> Array -> Result ScalarValue
asScalar _ (Array _ [x]) = pure x
asScalar e _ = err e

isEmpty :: Array -> Bool
isEmpty (Array sh _) = 0 `elem` sh

asVector :: Error -> Array -> Result [ScalarValue]
asVector _ (Array [] scalar) = pure scalar
asVector _ (Array [_] vec)   = pure vec
asVector e _                 = err e

onMajorCells ::
  ([Array] -> Result [Array])
  -> Array -> Result Array
onMajorCells f x = do
  result <- f $ majorCells x
  pure $ arrayReshaped (arrayShape x) $ concatMap arrayContents result

-- * Scalar functions

scalarMonad ::
  (ScalarValue -> Result ScalarValue)
      -> Array -> Result Array
scalarMonad f (Array sh cs) = Array sh <$> mapM f' cs where
  f' (Box xs) = Box <$> scalarMonad f xs
  f' x = f x

scalarDyad ::
  (ScalarValue -> ScalarValue -> Result ScalarValue)
      -> Array ->       Array -> Result Array
scalarDyad f a@(Array ash as) b@(Array bsh bs)
  | isScalar a && isScalar b = let ([a'], [b']) = (as, bs) in scalar <$> f' a' b'
  | isScalar a = let [a'] = as in Array bsh <$> mapM (a' `f'`) bs
  | isScalar b = let [b'] = bs in Array (arrayShape a) <$> mapM (`f'` b') (arrayContents a)
  | arrayShape a == arrayShape b =
    Array (arrayShape a) <$> zipWithM f' (arrayContents a) (arrayContents b)
  | otherwise = err $ DomainError "Mismatched left and right argument shapes"
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

monadN2N' = monadN2N . (pure .)

dyadNN2N f = scalarDyad f' where
  f' a b = do
    a' <- flip asNumber a $ DomainError "Expected number"
    b' <- flip asNumber b $ DomainError "Expected number"
    Number <$> f a' b'

dyadNN2N' = dyadNN2N . (pure .:)

monadB2B f = scalarMonad f' where
  f' x = do
    x' <- flip asBool x $ DomainError "Expected boolean"
    boolToScalar <$> f x'

monadB2B' = monadB2B . (pure .)

dyadBB2B f = scalarDyad f' where
  f' a b = do
    a' <- flip asBool a $ DomainError "Expected boolean"
    b' <- flip asBool b $ DomainError "Expected boolean"
    boolToScalar <$> f a' b'

dyadBB2B' = dyadBB2B . (pure .:)

instance Num Array where
  (+) = unerror .: dyadNN2N' (+)
  (-) = unerror .: dyadNN2N' (-)
  (*) = unerror .: dyadNN2N' (*)
  abs = unerror . monadN2N' abs
  signum = unerror . monadN2N' signum
  fromInteger = scalar . Number . fromInteger

instance Fractional Array where
  recip = unerror . monadN2N (\case
    0 -> err $ DomainError "Divide by zero"
    x -> pure $ recip x)
  (/) = unerror .: dyadNN2N (\cases
    0 0 -> pure 1
    _ 0 -> err $ DomainError "Divide by zero"
    x y -> pure $ x / y)
  fromRational = scalar . Number . fromRational

instance Floating Array where
  pi = scalar $ Number pi
  exp = unerror . monadN2N' exp
  log = unerror . monadN2N (\case
    0 -> err $ DomainError "Logarithm of zero"
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
    { dfnMonad :: Maybe (Array -> St Array)
    , dfnDyad  :: Maybe (Array -> Array -> St Array)
    , dfnRepr  :: String }
  | Atop { atopLeft :: Function, atopRight :: Function }
  | Over { overLeft :: Function, overRight :: Function }
  | After { afterLeft :: Function, afterRight :: Function }
  | Before { beforeLeft :: Function, beforeRight :: Function }
  | LeftHook { leftHookLeft :: Function, leftHookRight :: Function }
  | RightHook { rightHookLeft :: Function, rightHookRight :: Function }
  | Selfie { selfieFunction :: Function }
  | BindLeft { bindLeftArray :: Array, bindLeftFunction :: Function }
  | BindRight { bindRightFunction :: Function, bindRightArray :: Array }
  | DefaultBindLeft { defaultBindLeftArray :: Array, defaultBindLeftFunction :: Function }
  | DefaultBindRight { defaultBindRightFunction :: Function, defaultBindRightArray :: Array }
  | Constant { constantArray :: Array }

instance Show Function where
  show (DefinedFunction { dfnRepr = repr }) = repr
  show (l `Atop` r) = "(" ++ show l ++ [')', G.atop, '('] ++ show r ++ ")"
  show (l `Over` r) = "(" ++ show l ++ [')', G.over, '('] ++ show r ++ ")"
  show (l `After` r) = "(" ++ show l ++ [')', G.after, '('] ++ show r ++ ")"
  show (l `Before` r) = "(" ++ show l ++ [')', G.before, '('] ++ show r ++ ")"
  show (l `LeftHook` r) = "(" ++ show l ++ [')', G.leftHook, '('] ++ show r ++ ")"
  show (l `RightHook` r) = "(" ++ show l ++ [')', G.rightHook, '('] ++ show r ++ ")"
  show (Selfie f) = show f ++ [G.selfie]
  show (l `BindLeft` r) = "(" ++ show l ++ [')', G.after, '('] ++ show r ++ ")"
  show (l `BindRight` r) = "(" ++ show l ++ [')', G.after, '('] ++ show r ++ ")"
  show (l `DefaultBindLeft` r) = "(" ++ show l ++ [')', G.before, '('] ++ show r ++ ")"
  show (l `DefaultBindRight` r) = "(" ++ show l ++ [')', G.before, '('] ++ show r ++ ")"
  show (Constant x) = show x ++ [G.selfie]

callMonad :: Function -> Array -> St Array
callMonad (DefinedFunction (Just f) _ _) x = f x
callMonad f@(DefinedFunction Nothing _ _) _ = throwError $ DomainError $ "Function " ++ show f ++ " cannot be called monadically."
callMonad (f `Atop` g) x = callMonad g x >>= callMonad f
callMonad (f `Over` g) x = callMonad g x >>= callMonad f
callMonad (f `After` g) x = callMonad g x >>= callMonad f
callMonad (f `Before` g) x = callMonad f x >>= callMonad g
callMonad (f `LeftHook` g) x = do
  x' <- callMonad f x
  callDyad g x' x
callMonad (f `RightHook` g) x = do
  x' <- callMonad g x
  callDyad f x x'
callMonad (Selfie f) x = callDyad f x x
callMonad (a `BindLeft` f) x = callDyad f a x
callMonad (f `BindRight` a) x = callDyad f x a
callMonad (a `DefaultBindLeft` f) x = callDyad f a x
callMonad (f `DefaultBindRight` a) x = callDyad f x a
callMonad (Constant x) _ = pure x

callDyad :: Function -> Array -> Array -> St Array
callDyad (DefinedFunction _ (Just g) _) a b = g a b
callDyad f@(DefinedFunction _ Nothing _) _ _ = throwError $ DomainError $ "Function " ++ show f ++ " cannot be called dyadically."
callDyad (f `Atop` g) a b = callDyad g a b >>= callMonad f
callDyad (f `Over` g) a b = do
  a' <- callMonad g a
  b' <- callMonad g b
  callDyad f a' b'
callDyad (f `After` g) a b = do
  b' <- callMonad g b
  callDyad f a b'
callDyad (f `Before` g) a b = do
  a' <- callMonad f a
  callDyad g a' b
callDyad (f `LeftHook` g) a b = do
  a' <- callMonad f a
  callDyad g a' b
callDyad (f `RightHook` g) a b = do
  b' <- callMonad g b
  callDyad f a b'
callDyad (Selfie f) a b = callDyad f b a
callDyad (_ `BindLeft` _) _ _ = throwError $ DomainError "Bound function called dyadically"
callDyad (_ `BindRight` _) _ _ = throwError $ DomainError "Bound function called dyadically"
callDyad (_ `DefaultBindLeft` f) x y = callDyad f x y
callDyad (f `DefaultBindRight` _) x y = callDyad f x y
callDyad (Constant x) _ _ = pure x

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

-- * State

data Scope = Scope
  { scopeArrays :: [(String, Array)]
  , scopeFunctions :: [(String, Function)]
  , scopeAdverbs :: [(String, Adverb)]
  , scopeConjunctions :: [(String, Conjunction)]
  , scopeParent :: Maybe Scope }
  deriving (Show)

scopeLookupArray :: String -> Scope -> Maybe Array
scopeLookupArray name sc =
  lookup name (scopeArrays sc) <|> (scopeParent sc >>= scopeLookupArray name)

scopeLookupFunction :: String -> Scope -> Maybe Function
scopeLookupFunction name sc =
  lookup name (scopeFunctions sc) <|> (scopeParent sc >>= scopeLookupFunction name)

scopeLookupAdverb :: String -> Scope -> Maybe Adverb
scopeLookupAdverb name sc =
  lookup name (scopeAdverbs sc) <|> (scopeParent sc >>= scopeLookupAdverb name)

scopeLookupConjunction :: String -> Scope -> Maybe Conjunction
scopeLookupConjunction name sc =
  lookup name (scopeConjunctions sc) <|> (scopeParent sc >>= scopeLookupConjunction name)

scopeUpdateArray :: String -> Array -> Scope -> Scope
scopeUpdateArray name val sc = sc{ scopeArrays = update name val (scopeArrays sc) }

scopeUpdateFunction :: String -> Function -> Scope -> Scope
scopeUpdateFunction name val sc = sc{ scopeFunctions = update name val (scopeFunctions sc) }

scopeUpdateAdverb :: String -> Adverb -> Scope -> Scope
scopeUpdateAdverb name val sc = sc{ scopeAdverbs = update name val (scopeAdverbs sc) }

scopeUpdateConjunction :: String -> Conjunction -> Scope -> Scope
scopeUpdateConjunction name val sc = sc{ scopeConjunctions = update name val (scopeConjunctions sc) }

type St = StateT Scope (ExceptT Error IO)