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
import Data.Maybe (fromJust)

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
circle = pureFunction (Just $ monadN2N' (pi *)) (Just $ dyadNN2N $ \cases
  0     y -> pure $ sqrt $ 1 - y * y
  1     y -> pure $ sin y
  (-1)  y -> pure $ asin y
  2     y -> pure $ cos y
  (-2)  y -> pure $ acos y
  3     y -> pure $ tan y
  (-3)  y -> pure $ atan y
  4     y -> pure $ sqrt $ 1 + y * y
  (-4)  y -> pure $ sqrt $ y * y - 1
  5     y -> pure $ sinh y
  (-5)  y -> pure $ asinh y
  6     y -> pure $ cosh y
  (-6)  y -> pure $ acosh y
  7     y -> pure $ tanh y
  (-7)  y -> pure $ atanh y
  8     y -> pure $ sqrt $ negate $ 1 + y * y
  (-8)  y -> pure $ negate $ sqrt $ negate $ 1 + y * y
  9     y -> pure $ realPart y :+ 0
  (-9)  y -> pure y
  10    y -> pure $ Prelude.abs y
  (-10) y -> pure $ conjugate y
  11    y -> pure $ imagPart y :+ 0
  (-11) y -> pure $ y * (0 :+ 1)
  12    y -> pure $ Data.Complex.phase y :+ 0
  (-12) y -> pure $ exp $ y * (0 :+ 1)
  _     _ -> err $ DomainError "Invalid left argument to circular") [G.circle]
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
  if negative == 0 then case arrayReshaped (toEnum . fromEnum <$> shape) xs of
    Nothing -> err $ DomainError "Cannot reshape empty array to non-empty array"
    Just rs -> pure rs
  else if negative == 1 && (-1) `elem` shape then do
    let allElements = genericLength xs
    let known = product $ filter (>= 0) shape
    if known == 0 then err $ DomainError $ "Shape cannot and contain both 0 and " ++ [G.negative] ++ "1"
    else if allElements `mod` known /= 0 then err $ DomainError "Shape is not a multiple of the bound of the array"
    else case arrayReshaped (toEnum . fromEnum <$> map (\x -> if x == (-1) then allElements `div` known else x) shape) xs of
      Nothing -> err $ DomainError "Cannot reshape empty array to non-empty array"
      Just rs -> pure rs
  else err $ DomainError "Invalid shape"
  ) [G.rho]
ravel = pureFunction (Just $ pure . vector . arrayContents) Nothing [G.ravel]
reverse = pureFunction (Just $ onMajorCells $ pure . List.reverse) (Just $ \r arr -> let
  rotate c
    | c < 0 = List.reverse . rotate (negate c) . List.reverse
    | c == 0 = id
    | otherwise = \case
      []       -> []
      (x : xs) -> rotate (c - 1) (xs ++ [x])
  go []     xs = xs
  go (d:ds) xs = fromMajorCells $ rotate d $ go ds <$> majorCells xs
  mustBeIntegral = DomainError "Rotate left argument must be integral"
  in do
    rs <- asVector (RankError "Rotate left argument must be a vector") r >>= mapM (asNumber mustBeIntegral >=> asInt mustBeIntegral)
    pure $ go rs arr) [G.reverse]
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
  return $ fromJust $ arrayReshaped vec $ box . fromJust . arrayReshaped (arrayShape x) . fmap (Number . fromInteger . toEnum . fromEnum) <$> indices
  ) Nothing [G.iota]
indices = pureFunction (Just $ \(Array sh cs) -> do
  let error = DomainError "Indices requires an array of naturals"
  let indices = generateIndices sh
  let shape = if length sh == 1 then [] else [genericLength sh]
  let rep idx c = genericReplicate c $ box $ fromJust $ arrayReshaped shape $ Number . fromInteger . toEnum . fromEnum <$> idx
  counts <- mapM (asNumber error >=> asNat error) cs
  return $ vector $ concat $ zipWith rep indices counts
  ) Nothing [G.indices]
replicate = pureFunction Nothing (Just $ \r arr -> do
  let error = DomainError "Replicate left argument must be a natural vector"
  rs <- asVector error r >>= mapM (asNumber error >=> asNat error)
  let cells = majorCells arr
  if length rs /= length cells then err $ LengthError "Replicate: different lengths in left and right argument"
  else return $ fromMajorCells $ concat $ zipWith genericReplicate rs cells) [G.replicate]
abs = pureFunction (Just $ monadN2N' Prelude.abs) (Just $ dyadNN2N $ \x y ->
  if x == 0 then err $ DomainError "Remainder by zero"
  else return $ y - (fromInteger (Prelude.floor $ realPart $ y / x) :+ fromInteger (Prelude.floor $ imagPart $ y / x)) * x) [G.abs]
phase = pureFunction (Just $ monadN2N' $ \x -> Data.Complex.phase x :+ 0) (Just $ dyadNN2N' $ \x y -> Prelude.abs x * exp (0 :+ Data.Complex.phase y)) [G.phase]
real = pureFunction (Just $ monadN2N' $ \x -> realPart x :+ 0) (Just $ dyadNN2N' $ \x y -> realPart y :+ imagPart x) [G.real]
imag = pureFunction (Just $ monadN2N' $ \x -> imagPart x :+ 0) (Just $ dyadNN2N' $ \x y -> realPart x :+ imagPart y) [G.imag]

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
  , TinyAPL.Primitives.indices
  , TinyAPL.Primitives.replicate
  , TinyAPL.Primitives.abs
  , TinyAPL.Primitives.phase
  , TinyAPL.Primitives.real
  , TinyAPL.Primitives.imag ]

-- * Primitive adverbs

selfie = Adverb
  { adverbRepr = [G.selfie]
  , adverbOnArray = Just $ \x -> pure $ Constant x
  , adverbOnFunction = Just $ \f -> pure $ Selfie f }
reduceDown = Adverb
  { adverbRepr = [G.reduceDown]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ ReduceDown f }
reduceUp = Adverb
  { adverbRepr = [G.reduceUp]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ ReduceUp f }
scanDown = Adverb
  { adverbRepr = [G.scanDown]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ ScanDown f }
scanUp = Adverb
  { adverbRepr = [G.scanUp]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ ScanUp f }

adverbs = (\x -> (head $ adverbRepr x, x)) <$>
  [ TinyAPL.Primitives.selfie
  , TinyAPL.Primitives.reduceDown
  , TinyAPL.Primitives.reduceUp
  , TinyAPL.Primitives.scanDown
  , TinyAPL.Primitives.scanUp ]

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