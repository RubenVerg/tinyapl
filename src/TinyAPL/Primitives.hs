{-# LANGUAGE LambdaCase #-}
module TinyAPL.Primitives where
import TinyAPL.Array
import TinyAPL.Error
import TinyAPL.Function
import TinyAPL.Operator
import TinyAPL.Util
import qualified TinyAPL.Glyphs as G
import qualified Data.Ord as Ord
import Data.Complex
import Data.List
import qualified Data.List as List
import Control.Monad ((>=>))

-- * Primitive functions

plus = DefinedFunction (Just $ monadN2N' conjugate) (Just $ dyadNN2N' (+)) [G.plus]
minus = DefinedFunction (Just $ monadN2N' negate) (Just $ dyadNN2N' (-)) [G.minus]
times = DefinedFunction (Just $ monadN2N' signum) (Just $ dyadNN2N' (*)) [G.times]
divide = DefinedFunction (Just $ monadN2N $ \case
  0 -> err $ DomainError "Divide by zero"
  x -> pure $ recip x) (Just $ dyadNN2N $ \cases
  0 0 -> pure 1
  _ 0 -> err $ DomainError "Divide by zero"
  x y -> pure $ x / y) [G.divide]
power = DefinedFunction (Just $ monadN2N' exp) (Just $ dyadNN2N' (**)) [G.power]
logarithm = DefinedFunction (Just $ monadN2N $ \case
  0 -> err $ DomainError "Logarithm of zero"
  x -> pure $ log x) (Just $ dyadNN2N $ \cases
  1 1 -> pure 1
  1 _ -> err $ DomainError "Logarithm base one"
  _ 0 -> err $ DomainError "Logarithm of zero"
  x y -> pure $ logBase x y
  ) [G.logarithm]
circle = DefinedFunction (Just $ monadN2N' (pi *)) (Just $ dyadNN2N' $ \cases
  0     y -> sqrt $ 1 - y * y
  1     y -> sin y
  (-1)  y -> asin y
  2     y -> cos y
  (-2)  y -> acos y
  3     y -> tan y
  (-3)  y -> atan y
  4     y -> sqrt $ 1 + y * y
  (-4)  y -> sqrt $ y * y - 1
  5     y -> sinh y
  (-5)  y -> asinh y
  6     y -> cosh y
  (-6)  y -> acosh y
  7     y -> tanh y
  (-7)  y -> atanh y
  8     y -> sqrt $ negate $ 1 + y * y
  (-8)  y -> negate $ sqrt $ negate $ 1 + y * y
  9     y -> realPart y :+ 0
  (-9)  y -> y
  10    y -> abs y
  (-10) y -> conjugate y
  11    y -> imagPart y :+ 0
  (-11) y -> y * (0 :+ 1)
  12    y -> phase y :+ 0
  (-12) y -> exp $ y * (0 :+ 1)) [G.circle]
root = DefinedFunction (Just $ monadN2N' sqrt) (Just $ dyadNN2N' $ \x y -> x ** recip y) [G.root]
min = DefinedFunction Nothing (Just $ scalarDyad $ pure .: Ord.min) [G.floor]
max = DefinedFunction Nothing (Just $ scalarDyad $ pure .: Ord.max) [G.ceil]
less = DefinedFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (<)) [G.less]
lessEqual = DefinedFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (<=)) [G.lessEqual]
equal = DefinedFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (==)) [G.equal]
greaterEqual = DefinedFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (>=)) [G.greaterEqual]
greater = DefinedFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (>)) [G.greater]
notEqual = DefinedFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (/=)) [G.notEqual]
and = DefinedFunction Nothing (Just $ dyadBB2B' (&&)) [G.and]
or = DefinedFunction Nothing (Just $ dyadBB2B' (||)) [G.or]
nand = DefinedFunction Nothing (Just $ dyadBB2B' $ not .: (&&)) [G.nand]
nor = DefinedFunction Nothing (Just $ dyadBB2B' $ not .: (||)) [G.nor]
cartesian = DefinedFunction (Just $ monadN2N' (* (0 :+ 1))) (Just $ dyadNN2N' $ \x y -> x + (0 :+ 1) * y) [G.cartesian]
polar = DefinedFunction (Just $ monadN2N' $ exp . (* (0 :+ 1))) (Just $ dyadNN2N' $ \x y -> x * (exp y * (0 :+ 1))) [G.polar]
match = DefinedFunction Nothing (Just $ pure .: scalar .: boolToScalar .: (==)) [G.match]
notMatch = DefinedFunction (Just $ pure . genericLength . majorCells) (Just $ pure .: scalar .: boolToScalar .: (/=)) [G.notMatch]
rho = DefinedFunction (Just $ \(Array sh _) -> pure $ vector $ Number . fromInteger . toEnum . fromEnum <$> sh) (Just $ \sh (Array _ xs) -> do
  let mustBeIntegral = DomainError "Shape must be integral"
  shape <- (asVector (RankError "Shape must be a vector") sh >>= mapM (asNumber mustBeIntegral >=> asInt mustBeIntegral)) :: Result [Integer]
  let negative = count (< 0) shape
  if negative == 0 then pure $ Array (toEnum . fromEnum <$> shape) xs
  else if negative == 1 && (-1) `elem` shape then do
    let allElements = genericLength xs
    let known = product $ filter (>= 0) shape
    if known == 0 then err $ DomainError $ "Shape cannot and contain both 0 and " ++ [G.negative] ++ "1"
    else if allElements `mod` known /= 0 then err $ DomainError "Shape is not a multiple of the bound of the array"
    else pure $ Array (toEnum . fromEnum <$> map (\x -> if x == (-1) then allElements `div` known else x) shape) xs
  else err $ DomainError "Invalid shape"
  ) [G.rho]
ravel = DefinedFunction (Just $ pure . vector . arrayContents) Nothing [G.ravel]
reverse = DefinedFunction (Just $ onMajorCells $ pure . List.reverse) Nothing [G.reverse]
pair = DefinedFunction (Just $ pure . vector . singleton . box) (Just $ \x y -> pure $ vector [box x, box y]) [G.pair]
enclose = DefinedFunction (Just $ pure . scalar . box) Nothing [G.enclose]
first = DefinedFunction (Just $ \case
  Array _ [] -> err $ DomainError "First on empty array"
  Array _ (x:_) -> pure $ scalar x) Nothing [G.first]
last = DefinedFunction (Just $ \case
  Array _ [] -> err $ DomainError "Last on empty array"
  Array _ xs -> pure $ scalar $ List.last xs) Nothing [G.last]
take = DefinedFunction Nothing (Just $ \t arr -> let
  take' c = if c < 0 then List.reverse . genericTake (negate c) . List.reverse else genericTake c
  go []     xs = xs
  go (t:ts) xs = fromMajorCells $ take' t $ go ts <$> majorCells xs
  mustBeIntegral = DomainError "Take left argument must be integral"
  in do
    ts <- asVector (RankError "Take left argument must be a vector") t >>= mapM (asNumber mustBeIntegral >=> asInt mustBeIntegral)
    pure $ go ts arr
  ) [G.take]
drop = DefinedFunction Nothing (Just $ \d arr -> let
  drop' c = if c < 0 then List.reverse . genericDrop (negate c) . List.reverse else genericDrop c
  go []     xs = xs
  go (d:ds) xs = fromMajorCells $ drop' d $ go ds <$> majorCells xs
  mustBeIntegral = DomainError "Drop left argument must be integral"
  in do
    ds <- asVector (RankError "Drop left argument must be a vector") d >>= mapM (asNumber mustBeIntegral >=> asInt mustBeIntegral)
    pure $ go ds arr
  ) [G.drop]
left = DefinedFunction (Just $ \x -> pure x) (Just $ \x _ -> pure x) [G.left]
right = DefinedFunction (Just $ \x -> pure x) (Just $ \_ y -> pure y) [G.right]

-- * Primitive adverbs

-- * Primitive conjunctions

atop = Conjunction
  { conjRepr = [G.atop]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \_ _ -> err $ NYIError "Rank operator not implemented yet"
  , conjOnFunctionFunction = Just $ \f g -> pure $ f `Atop` g }
over = Conjunction
  { conjRepr = [G.over]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \_ _ -> err $ NYIError "Depth operator not implemented yet"
  , conjOnFunctionFunction = Just $ \f g -> pure $ f `Over` g }