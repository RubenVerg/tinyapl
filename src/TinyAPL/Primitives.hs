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

plus = Function (Just F.conjugate') (Just F.add') [G.plus]
minus = Function (Just F.neg') (Just F.sub') [G.minus]
times = Function (Just F.sign') (Just F.times') [G.times]
divide = Function (Just F.reciprocal') (Just F.divide') [G.divide]
power = Function (Just F.ePow') (Just F.pow') [G.power]
logarithm = Function (Just F.ln') (Just F.log') [G.logarithm]
circle = Function (Just F.piTimes') (Just F.circular') [G.circle]
root = Function (Just F.squareRoot') (Just F.root') [G.root]
floor = Function (Just F.floor') (Just F.min') [G.floor]
ceil = Function (Just F.ceil') (Just F.max') [G.ceil]
round = Function (Just F.round') Nothing [G.round]
less = Function Nothing (Just F.less') [G.less]
lessEqual = Function (Just $ F.sortUp') (Just F.lessEqual') [G.lessEqual]
equal = Function Nothing (Just F.equal') [G.equal]
greaterEqual = Function (Just $ F.sortDown') (Just F.greaterEqual') [G.greaterEqual]
greater = Function Nothing (Just F.greater') [G.greater]
notEqual = Function (Just F.nubSieve') (Just F.notEqual') [G.notEqual]
and = Function (Just F.promote) (Just F.lcm') [G.and]
or = Function (Just F.demote) (Just F.gcd') [G.or]
nand = Function Nothing (Just F.nand') [G.nand]
nor = Function Nothing (Just F.nor') [G.nor]
cartesian = Function (Just F.imaginary') (Just F.cartesian') [G.cartesian]
polar = Function (Just F.unitPolar') (Just F.polar') [G.polar]
identical = Function (Just F.depth') (Just F.identical') [G.identical]
notIdentical = Function (Just F.tally') (Just F.notIdentical') [G.notIdentical]
rho = Function (Just F.shape') (Just F.reshape') [G.rho]
ravel = Function (Just F.ravel') Nothing [G.ravel]
reverse = Function (Just F.reverse') (Just F.rotate') [G.reverse]
pair = Function (Just F.halfPair) (Just F.pair) [G.pair]
enclose = Function (Just F.enclose') Nothing [G.enclose]
first = Function (Just F.first) Nothing [G.first]
last = Function (Just F.last) (Just F.from) [G.last]
take = Function Nothing (Just F.take') [G.take]
drop = Function Nothing (Just F.drop') [G.drop]
left = Function (Just $ \x -> pure x) (Just $ \x _ -> pure x) [G.left]
right = Function (Just $ \x -> pure x) (Just $ \_ y -> pure y) [G.right]
iota = Function (Just F.indexGenerator') Nothing [G.iota]
indices = Function (Just F.indices) Nothing [G.indices]
replicate = Function Nothing (Just F.replicate') [G.replicate]
abs = Function (Just F.abs') (Just F.remainder') [G.abs]
phase = Function (Just F.phase') Nothing [G.phase]
real = Function (Just F.real') Nothing [G.real]
imag = Function (Just F.imag') Nothing [G.imag]
union = Function (Just F.unique') (Just F.union') [G.union]
intersection = Function Nothing (Just F.intersection') [G.intersection]
difference = Function (Just F.not') (Just F.difference') [G.difference]
symdiff = Function Nothing (Just F.symmetricDifference') [G.symdiff]
element = Function (Just F.enlist') Nothing [G.element]
roll = Function (Just F.roll') Nothing [G.roll]
squad = Function Nothing (Just $ F.squad) [G.squad]
rank = Function (Just F.rank') (Just F.rerank') [G.rank]
catenate = Function Nothing (Just F.catenate) [G.catenate]
gradeUp = Function (Just F.gradeUp') (Just F.sortByUp') [G.gradeUp]
gradeDown = Function (Just F.gradeDown') (Just F.sortByDown') [G.gradeDown]

functions = (\x -> (head $ functionRepr x, x)) <$>
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
  , TinyAPL.Primitives.identical
  , TinyAPL.Primitives.notIdentical
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
  , TinyAPL.Primitives.roll
  , TinyAPL.Primitives.squad
  , TinyAPL.Primitives.rank
  , TinyAPL.Primitives.catenate
  , TinyAPL.Primitives.gradeUp
  , TinyAPL.Primitives.gradeDown ]

-- * Primitive adverbs

selfie = Adverb
  { adverbRepr = [G.selfie]
  , adverbOnArray = Just $ \x -> pure $ Function (Just $ F.constant1 x) (Just $ F.constant2 x) (makeAdverbRepr (show x) G.selfie)
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.duplicate $ callDyad f) (Just $ F.commute $ callDyad f) (makeAdverbRepr (show f) G.selfie) }
reduce = Adverb
  { adverbRepr = [G.reduce]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.reduce' $ callDyad f) Nothing (makeAdverbRepr (show f) G.reduce) }
reduceBack = Adverb
  { adverbRepr = [G.reduceBack]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.reduceBack' $ callDyad f) Nothing (makeAdverbRepr (show f) G.reduceBack) }
onPrefixes = Adverb
  { adverbRepr = [G.onPrefixes]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.onPrefixes' $ callMonad f) Nothing (makeAdverbRepr (show f) G.onPrefixes) }
onSuffixes = Adverb
  { adverbRepr = [G.onSuffixes]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.onSuffixes' $ callMonad f) Nothing (makeAdverbRepr (show f) G.onSuffixes) }
each = Adverb
  { adverbRepr = [G.each]
  , adverbOnArray = Just $ \arr -> pure $ Function (Just $ F.each1 $ F.constant1 arr) (Just $ F.each2 $ F.constant2 arr) (makeAdverbRepr (show arr) G.each)
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.each1 $ callMonad f) (Just $ F.each2 $ callDyad f) (makeAdverbRepr (show f) G.each) }
eachLeft = Adverb
  { adverbRepr = [G.eachLeft]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function Nothing (Just $ F.eachLeft $ callDyad f) (makeAdverbRepr (show f) G.eachLeft) }
eachRight = Adverb
  { adverbRepr = [G.eachRight]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function Nothing (Just $ F.eachRight $ callDyad f) (makeAdverbRepr (show f) G.eachRight) }
key = Adverb
  { adverbRepr = [G.key]
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.keyMonad $ callDyad f) (Just $ F.key' $ callDyad f) (makeAdverbRepr (show f) G.key) }

adverbs = (\x -> (head $ adverbRepr x, x)) <$>
  [ TinyAPL.Primitives.selfie
  , TinyAPL.Primitives.reduce
  , TinyAPL.Primitives.reduceBack
  , TinyAPL.Primitives.onPrefixes
  , TinyAPL.Primitives.onSuffixes
  , TinyAPL.Primitives.each
  , TinyAPL.Primitives.eachLeft
  , TinyAPL.Primitives.eachRight
  , TinyAPL.Primitives.key ]

-- * Primitive conjunctions

atop = Conjunction
  { conjRepr = [G.atop]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \f r -> pure $ Function (Just $ F.atRank1' (callMonad f) r) (Just $ F.atRank2' (callDyad f) r) (makeConjRepr (show f) G.atop (show r))
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.compose (callMonad f) (callMonad g)) (Just $ F.atop (callMonad f) (callDyad g)) (makeConjRepr (show f) G.atop (show g)) }
over = Conjunction
  { conjRepr = [G.over]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \_ _ -> throwError $ NYIError "Depth operator not implemented yet"
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.compose (callMonad f) (callMonad g)) (Just $ F.over (callDyad f) (callMonad g)) (makeConjRepr (show f) G.over (show g)) }
after = Conjunction
  { conjRepr = [G.after]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Just $ \x f -> pure $ Function (Just $ \y -> callDyad f x y) Nothing (makeConjRepr (show x) G.after (show f))
  , conjOnFunctionArray = Just $ \f y -> pure $ Function (Just $ \x -> callDyad f x y) Nothing (makeConjRepr (show f) G.after (show y))
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.compose (callMonad f) (callMonad g)) (Just $ F.after (callDyad f) (callMonad g)) (makeConjRepr (show f) G.after (show g)) }
before = Conjunction
  { conjRepr = [G.before]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Just $ \x f -> pure $ Function (Just $ \y -> callDyad f x y) (Just $ \x' y -> callDyad f x' y) (makeConjRepr (show x) G.before (show f))
  , conjOnFunctionArray = Just $ \f y -> pure $ Function (Just $ \x -> callDyad f x y) (Just $ \x y' -> callDyad f x y') (makeConjRepr (show f) G.before (show y))
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.reverseCompose (callMonad f) (callMonad g)) (Just $ F.before (callMonad f) (callDyad g)) (makeConjRepr (show f) G.before (show g)) }
leftHook = Conjunction
  { conjRepr = [G.leftHook]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.leftHook (callMonad f) (callDyad g)) (Just $ F.before (callMonad f) (callDyad g)) (makeConjRepr (show f) G.leftHook (show g)) }
rightHook = Conjunction
  { conjRepr = [G.rightHook]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.rightHook (callDyad f) (callMonad g)) (Just $ F.after (callDyad f) (callMonad g)) (makeConjRepr (show f) G.rightHook (show g)) }
mirror = Conjunction
  { conjRepr = [G.mirror]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function Nothing (Just $ F.mirror (callDyad f) (callDyad g)) (makeConjRepr (show f) G.mirror (show g)) }
leftFork = Conjunction
  { conjRepr = [G.leftFork]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.leftHook (callMonad f) (callDyad g)) (Just $ F.leftFork (callDyad f) (callDyad g)) (makeConjRepr (show f) G.leftFork (show g)) }
rightFork = Conjunction
  { conjRepr = [G.rightFork]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.rightHook (callDyad f) (callMonad g)) (Just $ F.rightFork (callDyad f) (callDyad g)) (makeConjRepr (show f) G.rightFork (show g)) }
repeat = Conjunction
  { conjRepr = [G.repeat]
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \f y -> pure $ Function (Just $ F.repeat1 (callMonad f) y) (Just $ F.repeat2 (callDyad f) y) (makeConjRepr (show f) G.repeat (show y))
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.until1 (callMonad f) (callDyad g)) (Just $ F.until2 (callDyad f) (callDyad g)) (makeConjRepr (show f) G.repeat (show g)) }

conjunctions = (\x -> (head $ conjRepr x, x)) <$>
  [ TinyAPL.Primitives.atop
  , TinyAPL.Primitives.over
  , TinyAPL.Primitives.after
  , TinyAPL.Primitives.before
  , TinyAPL.Primitives.leftHook
  , TinyAPL.Primitives.rightHook
  , TinyAPL.Primitives.mirror
  , TinyAPL.Primitives.leftFork
  , TinyAPL.Primitives.rightFork
  , TinyAPL.Primitives.repeat ]