{-# LANGUAGE LambdaCase #-}
module TinyAPL.Primitives where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import TinyAPL.Util
import qualified TinyAPL.Glyphs as G
import qualified Data.Ord as Ord
import Data.Complex
import Data.List
import qualified Data.List as List
import Control.Monad ((>=>))
import Control.Monad.Except
import Control.Monad.State (lift)

pureFunction :: Maybe (Array -> Result Array) -> Maybe (Array -> Array -> Result Array) -> String -> Function
pureFunction m d = DefinedFunction
  ((\f x -> case f x of
    Left  e -> throwError e
    Right r -> return r) <$> m)
  ((\f x y -> case f x y of
    Left  e -> throwError e
    Right r -> return r) <$> d)

-- * Primitive arrays

zilde = vector []

arrays =
  [ (G.zilde, zilde) ]

-- * Primitive functions

plus = pureFunction (Just $ monadN2N' conjugate) (Just $ dyadNN2N' (+)) [G.plus]
minus = pureFunction (Just $ monadN2N' negate) (Just $ dyadNN2N' (-)) [G.minus]
times = pureFunction (Just $ monadN2N' signum) (Just $ dyadNN2N' (*)) [G.times]
divide = pureFunction (Just $ monadN2N $ \case
  0 -> err $ DomainError "Divide by zero"
  x -> pure $ recip x) (Just $ dyadNN2N $ \cases
  0 0 -> pure 1
  _ 0 -> err $ DomainError "Divide by zero"
  x y -> pure $ x / y) [G.divide]
power = pureFunction (Just $ monadN2N' exp) (Just $ dyadNN2N' (**)) [G.power]
logarithm = pureFunction (Just $ monadN2N $ \case
  0 -> err $ DomainError "Logarithm of zero"
  x -> pure $ log x) (Just $ dyadNN2N $ \cases
  1 1 -> pure 1
  1 _ -> err $ DomainError "Logarithm base one"
  _ 0 -> err $ DomainError "Logarithm of zero"
  x y -> pure $ logBase x y
  ) [G.logarithm]
circle = pureFunction (Just $ monadN2N' (pi *)) (Just $ dyadNN2N' $ \cases
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
root = pureFunction (Just $ monadN2N' sqrt) (Just $ dyadNN2N' $ \x y -> x ** recip y) [G.root]
floor = pureFunction (Just $ monadN2N' $ \(a :+ b) -> fromInteger (Prelude.floor a) :+ fromInteger (Prelude.floor b)) (Just $ scalarDyad $ pure .: Ord.min) [G.floor]
ceil = pureFunction (Just $ monadN2N' $ \(a :+ b) -> fromInteger (ceiling a) :+ fromInteger (ceiling b)) (Just $ scalarDyad $ pure .: Ord.max) [G.ceil]
round = pureFunction (Just $ monadN2N' $ \(a :+ b) -> let
  r x = Prelude.floor $ x + 0.5
  in fromInteger (r a) :+ fromInteger (r b)) Nothing [G.round]
less = pureFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (<)) [G.less]
lessEqual = pureFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (<=)) [G.lessEqual]
equal = pureFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (==)) [G.equal]
greaterEqual = pureFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (>=)) [G.greaterEqual]
greater = pureFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (>)) [G.greater]
notEqual = pureFunction Nothing (Just $ scalarDyad $ pure .: boolToScalar .: (/=)) [G.notEqual]
and = pureFunction Nothing (Just $ dyadBB2B' (&&)) [G.and]
or = pureFunction Nothing (Just $ dyadBB2B' (||)) [G.or]
nand = pureFunction Nothing (Just $ dyadBB2B' $ not .: (&&)) [G.nand]
nor = pureFunction Nothing (Just $ dyadBB2B' $ not .: (||)) [G.nor]
cartesian = pureFunction (Just $ monadN2N' (* (0 :+ 1))) (Just $ dyadNN2N' $ \x y -> x + (0 :+ 1) * y) [G.cartesian]
polar = pureFunction (Just $ monadN2N' $ exp . (* (0 :+ 1))) (Just $ dyadNN2N' $ \x y -> x * (exp y * (0 :+ 1))) [G.polar]
match = pureFunction Nothing (Just $ pure .: scalar .: boolToScalar .: (==)) [G.match]
notMatch = pureFunction (Just $ pure . genericLength . majorCells) (Just $ pure .: scalar .: boolToScalar .: (/=)) [G.notMatch]
rho = pureFunction (Just $ \(Array sh _) -> pure $ vector $ Number . fromInteger . toEnum . fromEnum <$> sh) (Just $ \sh (Array _ xs) -> do
  let mustBeIntegral = DomainError "Shape must be integral"
  shape <- (asVector (RankError "Shape must be a vector") sh >>= mapM (asNumber mustBeIntegral >=> asInt mustBeIntegral)) :: Result [Integer]
  let negative = count (< 0) shape
  if negative == 0 then pure $ arrayReshaped (toEnum . fromEnum <$> shape) xs
  else if negative == 1 && (-1) `elem` shape then do
    let allElements = genericLength xs
    let known = product $ filter (>= 0) shape
    if known == 0 then err $ DomainError $ "Shape cannot and contain both 0 and " ++ [G.negative] ++ "1"
    else if allElements `mod` known /= 0 then err $ DomainError "Shape is not a multiple of the bound of the array"
    else pure $ arrayReshaped (toEnum . fromEnum <$> map (\x -> if x == (-1) then allElements `div` known else x) shape) xs
  else err $ DomainError "Invalid shape"
  ) [G.rho]
ravel = pureFunction (Just $ pure . vector . arrayContents) Nothing [G.ravel]
reverse = pureFunction (Just $ onMajorCells $ pure . List.reverse) Nothing [G.reverse]
pair = pureFunction (Just $ pure . vector . singleton . box) (Just $ \x y -> pure $ vector [box x, box y]) [G.pair]
enclose = pureFunction (Just $ pure . scalar . box) Nothing [G.enclose]
first = pureFunction (Just $ \case
  Array _ [] -> err $ DomainError "First on empty array"
  Array _ (x:_) -> pure $ scalar x) Nothing [G.first]
last = pureFunction (Just $ \case
  Array _ [] -> err $ DomainError "Last on empty array"
  Array _ xs -> pure $ scalar $ List.last xs) Nothing [G.last]
take = pureFunction Nothing (Just $ \t arr -> let
  take' c = if c < 0 then List.reverse . genericTake (negate c) . List.reverse else genericTake c
  go []     xs = xs
  go (t:ts) xs = fromMajorCells $ take' t $ go ts <$> majorCells xs
  mustBeIntegral = DomainError "Take left argument must be integral"
  in do
    ts <- asVector (RankError "Take left argument must be a vector") t >>= mapM (asNumber mustBeIntegral >=> asInt mustBeIntegral)
    pure $ go ts arr
  ) [G.take]
drop = pureFunction Nothing (Just $ \d arr -> let
  drop' c = if c < 0 then List.reverse . genericDrop (negate c) . List.reverse else genericDrop c
  go []     xs = xs
  go (d:ds) xs = fromMajorCells $ drop' d $ go ds <$> majorCells xs
  mustBeIntegral = DomainError "Drop left argument must be integral"
  in do
    ds <- asVector (RankError "Drop left argument must be a vector") d >>= mapM (asNumber mustBeIntegral >=> asInt mustBeIntegral)
    pure $ go ds arr
  ) [G.drop]
left = pureFunction (Just $ \x -> pure x) (Just $ \x _ -> pure x) [G.left]
right = pureFunction (Just $ \x -> pure x) (Just $ \_ y -> pure y) [G.right]
iota = pureFunction (Just $ \x -> do
  let error = DomainError "Index Generator requires a vector (or scalar) of natural numbers"
  vec <- asVector error x >>= mapM (asNumber error >=> asNat error)
  let indices = generateIndices vec
  return $ arrayReshaped vec $ box . arrayReshaped (arrayShape x) . fmap (Number . fromInteger . toEnum . fromEnum) <$> indices
  ) Nothing [G.iota]
indices = pureFunction (Just $ \(Array sh cs) -> do
  let error = DomainError "Indices requires an array of naturals"
  let indices = generateIndices sh
  let shape = if length sh == 1 then [] else [genericLength sh]
  let rep idx c = genericReplicate c $ box $ arrayReshaped shape $ Number . fromInteger . toEnum . fromEnum <$> idx
  counts <- mapM (asNumber error >=> asNat error) cs
  return $ vector $ concat $ zipWith rep indices counts
  ) Nothing [G.indices]

functions = (\x -> (head $ dfnRepr x, x)) <$>
  [ TinyAPL.Primitives.plus
  , TinyAPL.Primitives.minus
  , TinyAPL.Primitives.times
  , TinyAPL.Primitives.divide
  , TinyAPL.Primitives.power
  , TinyAPL.Primitives.logarithm
  , TinyAPL.Primitives.circle
  , TinyAPL.Primitives.root
  , TinyAPL.Primitives.floor
  , TinyAPL.Primitives.ceil
  , TinyAPL.Primitives.round
  , TinyAPL.Primitives.less
  , TinyAPL.Primitives.lessEqual
  , TinyAPL.Primitives.equal
  , TinyAPL.Primitives.greaterEqual
  , TinyAPL.Primitives.greater
  , TinyAPL.Primitives.notEqual
  , TinyAPL.Primitives.and
  , TinyAPL.Primitives.or
  , TinyAPL.Primitives.nand
  , TinyAPL.Primitives.nor
  , TinyAPL.Primitives.cartesian
  , TinyAPL.Primitives.polar
  , TinyAPL.Primitives.match
  , TinyAPL.Primitives.notMatch
  , TinyAPL.Primitives.rho
  , TinyAPL.Primitives.ravel
  , TinyAPL.Primitives.reverse
  , TinyAPL.Primitives.pair
  , TinyAPL.Primitives.enclose
  , TinyAPL.Primitives.first
  , TinyAPL.Primitives.last
  , TinyAPL.Primitives.take
  , TinyAPL.Primitives.drop
  , TinyAPL.Primitives.left
  , TinyAPL.Primitives.right
  , TinyAPL.Primitives.iota
  , TinyAPL.Primitives.indices ]

-- * Primitive adverbs

selfie = Adverb
  { adverbRepr = [G.selfie]
  , adverbOnArray = Just $ \x -> pure $ Constant x
  , adverbOnFunction = Just $ \f -> pure $ Selfie f }

adverbs = (\x -> (head $ adverbRepr x, x)) <$>
  [ TinyAPL.Primitives.selfie ]

-- * Primitive conjunctions

atop = Conjunction
  { conjRepr = [G.atop]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \_ _ -> throwError $ NYIError "Rank operator not implemented yet"
  , conjOnFunctionFunction = Just $ \f g -> pure $ f `Atop` g }
over = Conjunction
  { conjRepr = [G.over]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \_ _ -> throwError $ NYIError "Depth operator not implemented yet"
  , conjOnFunctionFunction = Just $ \f g -> pure $ f `Over` g }
after = Conjunction
  { conjRepr = [G.after]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Just $ \a f -> pure $ a `BindLeft` f
  , conjOnFunctionArray = Just $ \f a -> pure $ f `BindRight` a
  , conjOnFunctionFunction = Just $ \f g -> pure $ f `After` g }
before = Conjunction
  { conjRepr = [G.before]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Just $ \a f -> pure $ a `DefaultBindLeft` f
  , conjOnFunctionArray = Just $ \f a -> pure $ f `DefaultBindRight` a
  , conjOnFunctionFunction = Just $ \f g -> pure $ f `Before` g }
leftHook = Conjunction
  { conjRepr = [G.leftHook]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ f `LeftHook` g }
rightHook = Conjunction
  { conjRepr = [G.rightHook]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ f `RightHook` g }

conjunctions = (\x -> (head $ conjRepr x, x)) <$>
  [ TinyAPL.Primitives.atop
  , TinyAPL.Primitives.over
  , TinyAPL.Primitives.after
  , TinyAPL.Primitives.before
  , TinyAPL.Primitives.leftHook
  , TinyAPL.Primitives.rightHook ]