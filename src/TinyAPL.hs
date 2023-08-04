{-# LANGUAGE LambdaCase #-}
module TinyAPL where
import qualified TinyAPL.Glyphs as G
import Data.Complex ( magnitude, realPart, Complex(..) )
import Numeric.Natural
import Data.List
import GHC.Stack (HasCallStack)
import Control.Monad

(.:) f g x y = f $ g x y

{-
Scalars:
 * complex numbers (internally represented as `Complex Double`s
 * characters
 * array boxes (enclosures)
-}

data ScalarValue
  = Number (Complex Double)
  | Character Char
  | Box Array

data Array = Array {
  arrayShape :: [Natural],
  arrayContents :: [ScalarValue]
}

-- Helper functions

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

-- Number comparison functions

comparisonTolerance = 1e-14

realEqual a b = abs (a - b) <= comparisonTolerance * (abs a `max` abs b)
complexEqual a b = magnitude (a - b) <= comparisonTolerance * (magnitude a `max` magnitude b)

isReal (_ :+ b) = 0 `realEqual` b -- A number is real if its imaginary part compares equal to zero.

-- Total ordering for scalars and arrays

instance Eq ScalarValue where
  (Character a) == (Character b) = a == b
  (Box as) == (Box bs) = as == bs
  (Number a) == (Number b)
    | isReal a && isReal b = realPart a `realEqual` realPart b
    | otherwise = a `complexEqual` b
  _ == _ = False

{-
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

showReal x = let
  isNegative = x < 0
  pos = if isInt x then show $ floor $ abs x else show $ abs x
  in if isNegative then G.negative : pos else pos

showComplex (a :+ b)
  | b `realEqual` 0 = showReal a
  | otherwise = showReal a ++ (G.imaginary : showReal b)

instance Show ScalarValue where
  show (Number x) = showComplex x
  show (Character x) = [x]
  show (Box xs) = "[box " ++ show xs ++ "]"

-- We'll implement proper array formatting later.
instance Show Array where
  show (Array sh cs) =
    "{ array with " ++ [G.rho] ++ " = " ++ unwords (map show sh) ++
    " and " ++ [G.ravel] ++ " = " ++ show cs ++ " }"

data Error
  = DomainError String
  | LengthError String
  | RankError String
  | NYIError String
  deriving (Show)

type Result = Either Error

-- sadly we need this.
unerror :: HasCallStack => Result a -> a
unerror (Right x) = x
unerror (Left e) = error $ show e

err :: Error -> Result a
err = Left

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

-- Scalar functions

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

monadN2N f = scalarMonad f' where
  f' x = do
    x' <- flip asNumber x $ DomainError ""
    Number <$> f x'

monadN2N' = monadN2N . (pure .)

dyadNN2N f = scalarDyad f' where
  f' a b = do
    a' <- flip asNumber a $ DomainError ""
    b' <- flip asNumber b $ DomainError ""
    Number <$> f a' b'

dyadNN2N' = dyadNN2N . (pure .:)

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