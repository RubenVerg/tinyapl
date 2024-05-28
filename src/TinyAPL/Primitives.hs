module TinyAPL.Primitives where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Functions as F
import qualified TinyAPL.Glyphs as G

-- * Primitive arrays

zilde = vector []

arrays =
  [ (G.zilde, zilde) ]

-- * Primitive functions

plus = DefinedFunction (Just F.conjugate') (Just F.add') [G.plus]
minus = DefinedFunction (Just F.neg') (Just F.sub') [G.minus]
times = DefinedFunction (Just F.sign') (Just F.times') [G.times]
divide = DefinedFunction (Just F.reciprocal') (Just F.divide') [G.divide]
power = DefinedFunction (Just F.ePow') (Just F.pow') [G.power]
logarithm = DefinedFunction (Just F.ln') (Just F.log') [G.logarithm]
circle = DefinedFunction (Just F.piTimes') (Just F.circular') [G.circle]
root = DefinedFunction (Just F.squareRoot') (Just F.root') [G.root]
floor = DefinedFunction (Just F.floor') (Just F.min') [G.floor]
ceil = DefinedFunction (Just F.ceil') (Just F.max') [G.ceil]
round = DefinedFunction (Just F.round') Nothing [G.round]
less = DefinedFunction Nothing (Just F.less') [G.less]
lessEqual = DefinedFunction Nothing (Just F.lessEqual') [G.lessEqual]
equal = DefinedFunction Nothing (Just F.equal') [G.equal]
greaterEqual = DefinedFunction Nothing (Just F.greaterEqual') [G.greaterEqual]
greater = DefinedFunction Nothing (Just F.greater') [G.greater]
notEqual = DefinedFunction (Just F.nubSieve') (Just F.notEqual') [G.notEqual]
and = DefinedFunction Nothing (Just F.lcm') [G.and]
or = DefinedFunction Nothing (Just F.gcd') [G.or]
nand = DefinedFunction Nothing (Just F.nand') [G.nand]
nor = DefinedFunction Nothing (Just F.nor') [G.nor]
cartesian = DefinedFunction (Just F.imaginary') (Just F.cartesian') [G.cartesian]
polar = DefinedFunction (Just F.unitPolar') (Just F.polar') [G.polar]
match = DefinedFunction Nothing (Just F.match') [G.match]
notMatch = DefinedFunction (Just F.tally') (Just F.notMatch') [G.notMatch]
rho = DefinedFunction (Just F.shape') (Just F.reshape') [G.rho]
ravel = DefinedFunction (Just F.ravel') Nothing [G.ravel]
reverse = DefinedFunction (Just F.reverse') (Just F.rotate') [G.reverse]
pair = DefinedFunction (Just F.halfPair) (Just F.pair) [G.pair]
enclose = DefinedFunction (Just F.enclose') Nothing [G.enclose]
first = DefinedFunction (Just F.first) Nothing [G.first]
last = DefinedFunction (Just F.last) Nothing [G.last]
take = DefinedFunction Nothing (Just F.take') [G.take]
drop = DefinedFunction Nothing (Just F.drop') [G.drop]
left = DefinedFunction (Just $ \x -> pure x) (Just $ \x _ -> pure x) [G.left]
right = DefinedFunction (Just $ \x -> pure x) (Just $ \_ y -> pure y) [G.right]
iota = DefinedFunction (Just F.indexGenerator') Nothing [G.iota]
indices = DefinedFunction (Just F.indices) Nothing [G.indices]
replicate = DefinedFunction Nothing (Just F.replicate') [G.replicate]
abs = DefinedFunction (Just F.abs') (Just F.remainder') [G.abs]
phase = DefinedFunction (Just F.phase') Nothing [G.phase]
real = DefinedFunction (Just F.real') Nothing [G.real]
imag = DefinedFunction (Just F.imag') Nothing [G.imag]
union = DefinedFunction (Just F.unique') (Just F.union') [G.union]
intersection = DefinedFunction Nothing (Just F.intersection') [G.intersection]
difference = DefinedFunction (Just F.not') (Just F.difference') [G.difference]
symdiff = DefinedFunction Nothing (Just F.symmetricDifference') [G.symdiff]
element = DefinedFunction (Just F.enlist') Nothing [G.element]
roll = DefinedFunction (Just F.roll') Nothing [G.roll]

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
  , TinyAPL.Primitives.imag
  , TinyAPL.Primitives.union
  , TinyAPL.Primitives.intersection
  , TinyAPL.Primitives.difference
  , TinyAPL.Primitives.symdiff
  , TinyAPL.Primitives.element
  , TinyAPL.Primitives.roll ]

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
each = Adverb
  { adverbRepr = [G.each]
  , adverbOnArray = Just $ \arr -> pure $ Each $ Constant arr
  , adverbOnFunction = Just $ \f -> pure $ Each f }
eachLeft = Adverb
  { adverbRepr = [G.eachLeft]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ EachLeft f }
eachRight = Adverb
  { adverbRepr = [G.eachRight]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ EachRight f }

adverbs = (\x -> (head $ adverbRepr x, x)) <$>
  [ TinyAPL.Primitives.selfie
  , TinyAPL.Primitives.reduceDown
  , TinyAPL.Primitives.reduceUp
  , TinyAPL.Primitives.scanDown
  , TinyAPL.Primitives.scanUp
  , TinyAPL.Primitives.each
  , TinyAPL.Primitives.eachLeft
  , TinyAPL.Primitives.eachRight ]

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