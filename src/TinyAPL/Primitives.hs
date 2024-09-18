module TinyAPL.Primitives where

import TinyAPL.ArrayFunctionOperator
import qualified TinyAPL.Functions as F
import qualified TinyAPL.Glyphs as G
import TinyAPL.Util (headPromise)

-- * Primitive arrays

zilde = vector []

arrays =
  [ (G.zilde, zilde) ]

-- * Primitive functions

plus = Function (Just F.conjugate') (Just F.add') [G.plus] Nothing
minus = Function (Just F.neg') (Just F.sub') [G.minus] Nothing
times = Function (Just F.sign') (Just F.times') [G.times] Nothing
divide = Function (Just F.reciprocal') (Just F.divide') [G.divide] Nothing
power = Function (Just F.ePow') (Just F.pow') [G.power] Nothing
logarithm = Function (Just F.ln') (Just F.log') [G.logarithm] Nothing
root = Function (Just F.squareRoot') (Just F.root') [G.root] Nothing
floor = Function (Just F.floor') (Just F.min') [G.floor] Nothing
ceil = Function (Just F.ceil') (Just F.max') [G.ceil] Nothing
round = Function (Just F.round') Nothing [G.round] Nothing
less = Function Nothing (Just F.less') [G.less] Nothing
lessEqual = Function Nothing (Just F.lessEqual') [G.lessEqual] Nothing
equal = Function Nothing (Just F.equal') [G.equal] Nothing
greaterEqual = Function Nothing (Just F.greaterEqual') [G.greaterEqual] Nothing
greater = Function Nothing (Just F.greater') [G.greater] Nothing
notEqual = Function (Just F.nubSieve') (Just F.notEqual') [G.notEqual] Nothing
and = Function (Just F.promote) (Just F.lcm') [G.and] Nothing
or = Function (Just F.demote) (Just F.gcd') [G.or] Nothing
nand = Function Nothing (Just F.nand') [G.nand] Nothing
nor = Function Nothing (Just F.nor') [G.nor] Nothing
cartesian = Function (Just F.imaginary') (Just F.cartesian') [G.cartesian] Nothing
polar = Function (Just F.unitPolar') (Just F.polar') [G.polar] Nothing
identical = Function (Just F.depth') (Just F.identical') [G.identical] Nothing
notIdentical = Function (Just F.tally') (Just F.notIdentical') [G.notIdentical] Nothing
rho = Function (Just F.shape') (Just F.reshape') [G.rho] Nothing
ravel = Function (Just F.ravel') (Just F.laminate) [G.ravel] Nothing
reverse = Function (Just F.reverse') (Just F.rotate') [G.reverse] Nothing
pair = Function (Just F.halfPair) (Just F.pair) [G.pair] Nothing
enclose = Function (Just F.enclose') Nothing [G.enclose] Nothing
first = Function (Just F.first) Nothing [G.first] Nothing
last = Function (Just F.last) (Just F.from) [G.last] Nothing
take = Function (Just F.mix) (Just F.take') [G.take] Nothing
drop = Function (Just F.majorCells') (Just F.drop') [G.drop] Nothing
left = Function (Just $ \x -> pure x) (Just $ \x _ -> pure x) [G.left] Nothing
right = Function (Just $ \x -> pure x) (Just $ \_ y -> pure y) [G.right] Nothing
iota = Function (Just F.indexGenerator') (Just F.indexOf) [G.iota] Nothing
indices = Function (Just F.indices) (Just F.intervalIndex) [G.indices] Nothing
replicate = Function Nothing (Just F.replicate') [G.replicate] Nothing
abs = Function (Just F.abs') (Just F.remainder') [G.abs] Nothing
phase = Function (Just F.phase') Nothing [G.phase] Nothing
real = Function (Just F.real') Nothing [G.real] Nothing
imag = Function (Just F.imag') Nothing [G.imag] Nothing
union = Function (Just F.unique') (Just F.union') [G.union] Nothing
intersection = Function Nothing (Just F.intersection') [G.intersection] Nothing
difference = Function (Just F.not') (Just F.difference') [G.difference] Nothing
symdiff = Function Nothing (Just F.symmetricDifference') [G.symdiff] Nothing
element = Function (Just F.enlist') (Just $ F.elementOf) [G.element] Nothing
roll = Function (Just F.roll') Nothing [G.roll] Nothing
squad = Function Nothing (Just $ F.squad) [G.squad] Nothing
rank = Function (Just F.rank') (Just F.rerank') [G.rank] Nothing
catenate = Function Nothing (Just F.catenate) [G.catenate] Nothing
gradeUp = Function (Just F.gradeUp') (Just F.sortByUp') [G.gradeUp] Nothing
gradeDown = Function (Just F.gradeDown') (Just F.sortByDown') [G.gradeDown] Nothing
precedes = Function Nothing (Just F.precedes') [G.precedes] Nothing
precedesOrIdentical = Function (Just $ F.sortUp') (Just F.precedesOrIdentical') [G.precedesOrIdentical] Nothing
succeedsOrIdentical = Function (Just $ F.sortDown') (Just F.succeedsOrIdentical') [G.succeedsOrIdentical] Nothing
succeeds = Function Nothing (Just F.succeeds') [G.succeeds] Nothing
minimal = Function Nothing (Just F.minimal) [G.minimal] Nothing
maximal = Function Nothing (Just F.maximal) [G.maximal] Nothing
transpose = Function (Just F.transpose) (Just F.reorderAxes') [G.transpose] Nothing
matrixInverse = Function (Just F.matrixInverse') (Just F.matrixDivide') [G.matrixInverse] Nothing
factorial = Function (Just F.factorial') (Just F.binomial') [G.factorial] Nothing
raise = Function (Just F.raise1) (Just F.raise') [G.raise] Nothing
decode = Function (Just F.decodeBase2) (Just F.decode') [G.decode] Nothing
encode = Function (Just F.encodeBase2) (Just F.encode') [G.encode] Nothing
histogram = Function Nothing (Just F.count) [G.histogram] Nothing

functions = (\x -> (headPromise $ functionRepr x, x)) <$>
  [ TinyAPL.Primitives.plus
  , TinyAPL.Primitives.minus
  , TinyAPL.Primitives.times
  , TinyAPL.Primitives.divide
  , TinyAPL.Primitives.power
  , TinyAPL.Primitives.logarithm
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
  , TinyAPL.Primitives.gradeDown
  , TinyAPL.Primitives.precedes
  , TinyAPL.Primitives.precedesOrIdentical
  , TinyAPL.Primitives.succeedsOrIdentical
  , TinyAPL.Primitives.succeeds
  , TinyAPL.Primitives.minimal
  , TinyAPL.Primitives.maximal
  , TinyAPL.Primitives.transpose
  , TinyAPL.Primitives.matrixInverse
  , TinyAPL.Primitives.factorial
  , TinyAPL.Primitives.raise
  , TinyAPL.Primitives.decode
  , TinyAPL.Primitives.encode
  , TinyAPL.Primitives.histogram ]

-- * Primitive adverbs

selfie = Adverb
  { adverbRepr = [G.selfie]
  , adverbContext = Nothing
  , adverbOnArray = Just $ \x -> pure $ Function (Just $ F.constant1 x) (Just $ F.constant2 x) (makeAdverbRepr (show x) G.selfie) Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.duplicate $ callDyad f) (Just $ F.commute $ callDyad f) (makeAdverbRepr (show f) G.selfie) Nothing }
reduce = Adverb
  { adverbRepr = [G.reduce]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.reduce' $ callDyad f) (Just $ F.fold' $ callDyad f) (makeAdverbRepr (show f) G.reduce) Nothing }
reduceBack = Adverb
  { adverbRepr = [G.reduceBack]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.reduceBack' $ callDyad f) (Just $ F.foldBack' $ callDyad f) (makeAdverbRepr (show f) G.reduceBack) Nothing }
onPrefixes = Adverb
  { adverbRepr = [G.onPrefixes]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.onPrefixes' $ callMonad f) Nothing (makeAdverbRepr (show f) G.onPrefixes) Nothing }
onSuffixes = Adverb
  { adverbRepr = [G.onSuffixes]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.onSuffixes' $ callMonad f) Nothing (makeAdverbRepr (show f) G.onSuffixes) Nothing }
each = Adverb
  { adverbRepr = [G.each]
  , adverbContext = Nothing
  , adverbOnArray = Just $ \x -> pure $ Function (Just $ F.each1 $ F.constant1 x) (Just $ F.each2 $ F.constant2 x) (makeAdverbRepr (show x) G.each) Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.each1 $ callMonad f) (Just $ F.each2 $ callDyad f) (makeAdverbRepr (show f) G.each) Nothing }
eachLeft = Adverb
  { adverbRepr = [G.eachLeft]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function Nothing (Just $ F.eachLeft $ callDyad f) (makeAdverbRepr (show f) G.eachLeft) Nothing }
eachRight = Adverb
  { adverbRepr = [G.eachRight]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function Nothing (Just $ F.eachRight $ callDyad f) (makeAdverbRepr (show f) G.eachRight) Nothing }
key = Adverb
  { adverbRepr = [G.key]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.keyMonad $ callDyad f) (Just $ F.key' $ callDyad f) (makeAdverbRepr (show f) G.key) Nothing }
onCells = Adverb
  { adverbRepr = [G.onCells]
  , adverbContext = Nothing
  , adverbOnArray = Just $ \x -> pure $ Function (Just $ F.onCells1 $ F.constant1 x) (Just $ F.onCells2 $ F.constant2 x) (makeAdverbRepr (show x) G.onCells) Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.onCells1 $ callMonad f) (Just $ F.onCells2 $ callDyad f) (makeAdverbRepr (show f) G.onCells) Nothing }
onScalars = Adverb
  { adverbRepr = [G.onScalars]
  , adverbContext = Nothing
  , adverbOnArray = Just $ \x -> pure $ Function (Just $ F.onScalars1 $ F.constant1 x) (Just $ F.onScalars2 $ F.constant2 x) (makeAdverbRepr (show x) G.onScalars) Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.onScalars1 $ callMonad f) (Just $ F.onScalars2 $ callDyad f) (makeAdverbRepr (show f) G.onScalars) Nothing }
boxed = Adverb
  { adverbRepr = [G.boxed]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.boxed1 $ callMonad f) (Just $ F.boxed2 $ callDyad f) (makeAdverbRepr (show f) G.boxed) Nothing }
onContents = Adverb
  { adverbRepr = [G.onContents]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ F.onContents1 $ callMonad f) (Just $ F.onContents2 $ callDyad f) (makeAdverbRepr (show f) G.onContents) Nothing }
table = Adverb
  { adverbRepr = [G.table]
  , adverbContext = Nothing
  , adverbOnArray = Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function Nothing (Just $ F.table (callDyad f)) (makeAdverbRepr (show f) G.table) Nothing }
ident = Adverb
  { adverbRepr = [G.ident]
  , adverbContext = Nothing
  , adverbOnArray = Just $ \n -> pure $ Function (Just $ F.constant1 n) (Just $ F.constant2 n) (makeAdverbRepr (show n) G.ident) Nothing
  , adverbOnFunction = Just $ \f -> pure $ Function (Just $ callMonad f) (Just $ callDyad f) (makeAdverbRepr (show f) G.ident) Nothing }

adverbs = (\x -> (headPromise $ adverbRepr x, x)) <$>
  [ TinyAPL.Primitives.selfie
  , TinyAPL.Primitives.reduce
  , TinyAPL.Primitives.reduceBack
  , TinyAPL.Primitives.onPrefixes
  , TinyAPL.Primitives.onSuffixes
  , TinyAPL.Primitives.each
  , TinyAPL.Primitives.eachLeft
  , TinyAPL.Primitives.eachRight
  , TinyAPL.Primitives.key
  , TinyAPL.Primitives.onCells
  , TinyAPL.Primitives.onScalars
  , TinyAPL.Primitives.boxed
  , TinyAPL.Primitives.onContents
  , TinyAPL.Primitives.table
  , TinyAPL.Primitives.ident ]

-- * Primitive conjunctions

atop = Conjunction
  { conjRepr = [G.atop]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \f r -> pure $ Function (Just $ F.atRank1' (callMonad f) r) (Just $ F.atRank2' (callDyad f) r) (makeConjRepr (show f) G.atop (show r)) Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.compose (callMonad f) (callMonad g)) (Just $ F.atop (callMonad f) (callDyad g)) (makeConjRepr (show f) G.atop (show g)) Nothing }
over = Conjunction
  { conjRepr = [G.over]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \f d -> pure $ Function (Just $ F.atDepth1' (callMonad f) d) (Just $ F.atDepth2' (callDyad f) d) (makeConjRepr (show f) G.over (show d)) Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.compose (callMonad f) (callMonad g)) (Just $ F.over (callDyad f) (callMonad g)) (makeConjRepr (show f) G.over (show g)) Nothing }
after = Conjunction
  { conjRepr = [G.after]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Just $ \x f -> pure $ Function (Just $ \y -> callDyad f x y) Nothing (makeConjRepr (show x) G.after (show f)) Nothing
  , conjOnFunctionArray = Just $ \f y -> pure $ Function (Just $ \x -> callDyad f x y) Nothing (makeConjRepr (show f) G.after (show y)) Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.compose (callMonad f) (callMonad g)) (Just $ F.after (callDyad f) (callMonad g)) (makeConjRepr (show f) G.after (show g)) Nothing }
before = Conjunction
  { conjRepr = [G.before]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Just $ \x f -> pure $ Function (Just $ \y -> callDyad f x y) (Just $ \x' y -> callDyad f x' y) (makeConjRepr (show x) G.before (show f)) Nothing
  , conjOnFunctionArray = Just $ \f y -> pure $ Function (Just $ \x -> callDyad f x y) (Just $ \x y' -> callDyad f x y') (makeConjRepr (show f) G.before (show y)) Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.reverseCompose (callMonad f) (callMonad g)) (Just $ F.before (callMonad f) (callDyad g)) (makeConjRepr (show f) G.before (show g)) Nothing }
leftHook = Conjunction
  { conjRepr = [G.leftHook]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.leftHook (callMonad f) (callDyad g)) (Just $ F.before (callMonad f) (callDyad g)) (makeConjRepr (show f) G.leftHook (show g)) Nothing }
rightHook = Conjunction
  { conjRepr = [G.rightHook]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.rightHook (callDyad f) (callMonad g)) (Just $ F.after (callDyad f) (callMonad g)) (makeConjRepr (show f) G.rightHook (show g)) Nothing }
mirror = Conjunction
  { conjRepr = [G.mirror]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function Nothing (Just $ F.mirror (callDyad f) (callDyad g)) (makeConjRepr (show f) G.mirror (show g)) Nothing }
leftFork = Conjunction
  { conjRepr = [G.leftFork]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.leftHook (callMonad f) (callDyad g)) (Just $ F.leftFork (callDyad f) (callDyad g)) (makeConjRepr (show f) G.leftFork (show g)) Nothing }
rightFork = Conjunction
  { conjRepr = [G.rightFork]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.rightHook (callDyad f) (callMonad g)) (Just $ F.rightFork (callDyad f) (callDyad g)) (makeConjRepr (show f) G.rightFork (show g)) Nothing }
repeat = Conjunction
  { conjRepr = [G.repeat]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Just $ \f y -> pure $ Function (Just $ F.repeat1 (callMonad f) y) (Just $ F.repeat2 (callDyad f) y) (makeConjRepr (show f) G.repeat (show y)) Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.until1 (callMonad f) (callDyad g)) (Just $ F.until2 (callDyad f) (callDyad g)) (makeConjRepr (show f) G.repeat (show g)) Nothing }
valences = Conjunction
  { conjRepr = [G.valences]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ callMonad f) (Just $ callDyad g) (makeConjRepr (show f) G.valences (show g)) Nothing }
under = Conjunction
  { conjRepr = [G.under]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Just $ \x g -> pure $ Function (Just $ F.underK x (callMonad g)) Nothing (makeConjRepr (show x) G.under (show g)) Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ F.under (callMonad f) (callMonad g)) (Just $ F.under2 (callDyad f) (callMonad g)) (makeConjRepr (show f) G.under (show g)) Nothing }
innerProduct = Conjunction
  { conjRepr = [G.innerProduct]
  , conjContext = Nothing
  , conjOnArrayArray = Nothing
  , conjOnArrayFunction = Nothing
  , conjOnFunctionArray = Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function Nothing (Just $ F.innerProduct (callMonad f) (callDyad g)) (makeConjRepr (show f) G.innerProduct (show g)) Nothing }
lev = Conjunction
  { conjRepr = [G.lev]
  , conjContext = Nothing
  , conjOnArrayArray = Just $ \n m -> pure $ Function (Just $ F.constant1 n) (Just $ F.constant2 n) (makeConjRepr (show n) G.lev (show m)) Nothing
  , conjOnArrayFunction = Just $ \n g -> pure $ Function (Just $ F.constant1 n) (Just $ F.constant2 n) (makeConjRepr (show n) G.lev (show g)) Nothing
  , conjOnFunctionArray = Just $ \f m -> pure $ Function (Just $ callMonad f) (Just $ callDyad f) (makeConjRepr (show f) G.lev (show m)) Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ callMonad f) (Just $ callDyad f) (makeConjRepr (show f) G.lev (show g)) Nothing }
dex = Conjunction
  { conjRepr = [G.dex]
  , conjContext = Nothing
  , conjOnArrayArray = Just $ \n m -> pure $ Function (Just $ F.constant1 m) (Just $ F.constant2 m) (makeConjRepr (show n) G.dex (show m)) Nothing
  , conjOnArrayFunction = Just $ \n g -> pure $ Function (Just $ callMonad g) (Just $ callDyad g) (makeConjRepr (show n) G.dex (show g)) Nothing
  , conjOnFunctionArray = Just $ \f m -> pure $ Function (Just $ F.constant1 m) (Just $ F.constant2 m) (makeConjRepr (show f) G.dex (show m)) Nothing
  , conjOnFunctionFunction = Just $ \f g -> pure $ Function (Just $ callMonad g) (Just $ callDyad g) (makeConjRepr (show f) G.dex (show g)) Nothing }

conjunctions = (\x -> (headPromise $ conjRepr x, x)) <$>
  [ TinyAPL.Primitives.atop
  , TinyAPL.Primitives.over
  , TinyAPL.Primitives.after
  , TinyAPL.Primitives.before
  , TinyAPL.Primitives.leftHook
  , TinyAPL.Primitives.rightHook
  , TinyAPL.Primitives.mirror
  , TinyAPL.Primitives.leftFork
  , TinyAPL.Primitives.rightFork
  , TinyAPL.Primitives.repeat
  , TinyAPL.Primitives.valences
  , TinyAPL.Primitives.under
  , TinyAPL.Primitives.innerProduct
  , TinyAPL.Primitives.lev
  , TinyAPL.Primitives.dex ]