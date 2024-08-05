{-# LANGUAGE FlexibleContexts, LambdaCase, NegativeLiterals #-}

module TinyAPL.Functions where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import TinyAPL.Random
import TinyAPL.Util

import Control.Monad.Except (MonadError)
import qualified Data.Complex as Cx
import Data.Complex ( Complex((:+)) )
import Data.Char (ord, chr)
import Data.Maybe (fromJust)
import Data.List (elemIndex, genericLength, genericTake, genericDrop, genericReplicate, nub, genericIndex, singleton)
import Numeric.Natural (Natural)
import Control.Monad
import Control.Monad.State (MonadIO)

-- * Functions

expectedNumber = DomainError "Expected number"
expectedReal = DomainError "Expected real"
expectedInteger = DomainError "Expected integer"
expectedNatural = DomainError "Expected natural"
expectedBool = DomainError "Expected boolean"

conjugate :: MonadError Error m => ScalarValue -> m ScalarValue
conjugate (Number y) = pure $ Number $ Cx.conjugate y
conjugate _ = throwError expectedNumber

conjugate' :: MonadError Error m => Array -> m Array
conjugate' = scalarMonad conjugate

add :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
add (Number x) (Number y) = pure $ Number $ x + y
add (Number x) (Character y) = do
  x' <- asInt expectedInteger x
  pure $ Character $ chr $ ord y + x'
add (Character x) (Number y) = do
  y' <- asInt expectedInteger y
  pure $ Character $ chr $ ord x + y'
add _ _ = throwError expectedNumber

add' :: MonadError Error m => Array -> Array -> m Array
add' = scalarDyad add

neg :: MonadError Error m => ScalarValue -> m ScalarValue
neg (Number y) = pure $ Number $ negate y
neg _ = throwError expectedNumber

neg' :: MonadError Error m => Array -> m Array
neg' = scalarMonad neg

sub :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
sub (Number x) (Number y) = pure $ Number $ x - y
sub (Character x) (Number y) = do
  y' <- asInt expectedInteger y
  pure $ Character $ chr $ ord x - y'
sub (Character x) (Character y) = pure $ Number $ fromInteger . toInteger $ ord x - ord y
sub _ _ = throwError expectedNumber

sub' :: MonadError Error m => Array -> Array -> m Array
sub' = scalarDyad sub

sign :: MonadError Error m => ScalarValue -> m ScalarValue
sign (Number y) = pure $ Number $ signum y
sign _ = throwError expectedNumber

sign' :: MonadError Error m => Array -> m Array
sign' = scalarMonad sign

times :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
times (Number x) (Number y) = pure $ Number $ x * y
times _ _ = throwError expectedNumber

times' :: MonadError Error m => Array -> Array -> m Array
times' = scalarDyad times

reciprocal :: MonadError Error m => ScalarValue -> m ScalarValue
reciprocal (Number 0) = throwError $ DomainError "Divide by zero"
reciprocal (Number y) = pure $ Number $ recip y
reciprocal _ = throwError expectedNumber

reciprocal' :: MonadError Error m => Array -> m Array
reciprocal' = scalarMonad reciprocal

divide :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
divide (Number 0) (Number 0) = pure $ Number 1
divide _ (Number 0) = throwError $ DomainError "Divide by zero"
divide (Number x) (Number y) = pure $ Number $ x / y
divide _ _ = throwError expectedNumber

divide' :: MonadError Error m => Array -> Array -> m Array
divide' = scalarDyad divide

ePow :: MonadError Error m => ScalarValue -> m ScalarValue
ePow (Number y) = pure $ Number $ exp y
ePow _ = throwError expectedNumber

ePow' :: MonadError Error m => Array -> m Array
ePow' = scalarMonad ePow

pow :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
pow (Number x) (Number y) = case asInt (DomainError "") y of
  Left _ -> pure $ Number $ x ** y
  Right y' -> pure $ Number $ x ^ y'
pow _ _ = throwError expectedNumber

pow' :: MonadError Error m => Array -> Array -> m Array
pow' = scalarDyad pow

ln :: MonadError Error m => ScalarValue -> m ScalarValue
ln (Number 0) = throwError $ DomainError "Logarithm of zero"
ln (Number y) = pure $ Number $ Prelude.log y
ln _ = throwError expectedNumber

ln' :: MonadError Error m => Array -> m Array
ln' = scalarMonad ln

log :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
log (Number 1) (Number 1) = pure $ Number 1
log (Number 1) _ = throwError $ DomainError "Logarithm base one"
log _ (Number 0) = throwError $ DomainError "Logarithm of zero"
log (Number x) (Number y) = pure $ Number $ logBase x y
log _ _ = throwError expectedNumber

log' :: MonadError Error m => Array -> Array -> m Array
log' = scalarDyad TinyAPL.Functions.log

squareRoot :: MonadError Error m => ScalarValue -> m ScalarValue
squareRoot (Number y) = pure $ Number $ sqrt y
squareRoot _ = throwError expectedNumber

squareRoot' :: MonadError Error m => Array -> m Array
squareRoot' = scalarMonad squareRoot

root :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
root (Number x) (Number y) = pure $ Number $ y ** recip x
root _ _ = throwError expectedNumber

root' :: MonadError Error m => Array -> Array -> m Array
root' = scalarDyad root

piTimes :: MonadError Error m => ScalarValue -> m ScalarValue
piTimes (Number y) = pure $ Number $ pi * y
piTimes _ = throwError expectedNumber

piTimes' :: MonadError Error m => Array -> m Array
piTimes' = scalarMonad piTimes

circular :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
circular (Number 0) (Number y) = pure $ Number $ sqrt $ 1 - y * y
circular (Number 1) (Number y) = pure $ Number $ sin y
circular (Number -1) (Number y) = pure $ Number $ asin y
circular (Number 2) (Number y) = pure $ Number $ cos y
circular (Number -2) (Number y) = pure $ Number $ acos y
circular (Number 3) (Number y) = pure $ Number $ tan y
circular (Number -3) (Number y) = pure $ Number $ atan y
circular (Number 4) (Number y) = pure $ Number $ sqrt $ 1 + y * y
circular (Number -4) (Number y) = pure $ Number $ sqrt $ y * y - 1
circular (Number 5) (Number y) = pure $ Number $ sinh y
circular (Number -5) (Number y) = pure $ Number $ asinh y
circular (Number 6) (Number y) = pure $ Number $ cosh y
circular (Number -6) (Number y) = pure $ Number $ acosh y
circular (Number 7) (Number y) = pure $ Number $ tanh y
circular (Number -7) (Number y) = pure $ Number $ atanh y
circular (Number 8) (Number y) = pure $ Number $ sqrt $ negate $ 1 + y * y
circular (Number -8) (Number y) = pure $ Number $ negate $ sqrt $ negate $ 1 + y * y
circular (Number 9) (Number y) = pure $ Number $ Cx.realPart y :+ 0
circular (Number -9) (Number y) = pure $ Number y
circular (Number 10) (Number y) = pure $ Number $ Prelude.abs y
circular (Number -10) (Number y) = pure $ Number $ Cx.conjugate y
circular (Number 11) (Number y) = pure $ Number $ Cx.imagPart y :+ 0
circular (Number -11) (Number y) = pure $ Number $ y * i
circular (Number 12) (Number y) = pure $ Number $ Cx.phase y :+ 0
circular (Number -12) (Number y) = pure $ Number $ exp $ y * i
circular (Number _) (Number _) = throwError $ DomainError "Invalid left argument to circular"
circular _ _ = throwError expectedNumber

circular' :: MonadError Error m => Array -> Array -> m Array
circular' = scalarDyad circular

floor :: MonadError Error m => ScalarValue -> m ScalarValue
floor (Number y) = pure $ Number $ complexFloor y
floor _ = throwError expectedNumber

floor' :: MonadError Error m => Array -> m Array
floor' = scalarMonad TinyAPL.Functions.floor

ceil :: MonadError Error m => ScalarValue -> m ScalarValue
ceil (Number y) = pure $ Number $ complexCeiling y
ceil _ = throwError expectedNumber

ceil' :: MonadError Error m => Array -> m Array
ceil' = scalarMonad ceil

round :: MonadError Error m => ScalarValue -> m ScalarValue
round (Number y) = pure $ Number $ componentFloor $ y + (0.5 :+ 0.5)
round _ = throwError expectedNumber

round' :: MonadError Error m => Array -> m Array
round' = scalarMonad TinyAPL.Functions.round

min :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
min x y = pure $ Prelude.min x y

min' :: MonadError Error m => Array -> Array -> m Array
min' = scalarDyad TinyAPL.Functions.min

max :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
max x y = pure $ Prelude.max x y

max' :: MonadError Error m => Array -> Array -> m Array
max' = scalarDyad TinyAPL.Functions.max

lcm :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
lcm (Number x) (Number y) = pure $ Number $ complexLCM x y
lcm _ _ = throwError expectedNumber

lcm' :: MonadError Error m => Array -> Array -> m Array
lcm' = scalarDyad TinyAPL.Functions.lcm

gcd :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
gcd (Number x) (Number y) = pure $ Number $ complexGCD x y
gcd _ _ = throwError expectedNumber

gcd' :: MonadError Error m => Array -> Array -> m Array
gcd' = scalarDyad TinyAPL.Functions.gcd

nand :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
nand (Number 0) (Number 0) = pure $ Number 1
nand (Number 0) (Number 1) = pure $ Number 1
nand (Number 1) (Number 0) = pure $ Number 1
nand (Number 1) (Number 1) = pure $ Number 0
nand _ _ = throwError expectedBool

nand' :: MonadError Error m => Array -> Array -> m Array
nand' = scalarDyad nand

nor :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
nor (Number 0) (Number 0) = pure $ Number 1
nor (Number 0) (Number 1) = pure $ Number 0
nor (Number 1) (Number 0) = pure $ Number 0
nor (Number 1) (Number 1) = pure $ Number 0
nor _ _ = throwError expectedBool

nor' :: MonadError Error m => Array -> Array -> m Array
nor' = scalarDyad nor

imaginary :: MonadError Error m => ScalarValue -> m ScalarValue
imaginary (Number y) = pure $ Number $ y * i
imaginary _ = throwError expectedNumber

imaginary' :: MonadError Error m => Array -> m Array
imaginary' = scalarMonad imaginary

cartesian :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
cartesian (Number x) (Number y) = pure $ Number $ x + y * i
cartesian _ _ = throwError expectedNumber

cartesian' :: MonadError Error m => Array -> Array -> m Array
cartesian' = scalarDyad cartesian

unitPolar :: MonadError Error m => ScalarValue -> m ScalarValue
unitPolar (Number y) = pure $ Number $ exp $ i * y
unitPolar _ = throwError expectedNumber

unitPolar' :: MonadError Error m => Array -> m Array
unitPolar' = scalarMonad unitPolar

polar :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
polar (Number x) (Number y) = pure $ Number $ x * exp (i * y)
polar _ _ = throwError expectedNumber

polar' :: MonadError Error m => Array -> Array -> m Array
polar' = scalarDyad polar

abs :: MonadError Error m => ScalarValue -> m ScalarValue
abs (Number y) = pure $ Number $ Prelude.abs y
abs _ = throwError expectedNumber

abs' :: MonadError Error m => Array -> m Array
abs' = scalarMonad TinyAPL.Functions.abs

remainder :: MonadError Error m => ScalarValue -> ScalarValue -> m ScalarValue
remainder (Number x) (Number y) = pure $ Number $ complexRemainder x y
remainder _ _ = throwError expectedNumber

remainder' :: MonadError Error m => Array -> Array -> m Array
remainder' = scalarDyad remainder

phase :: MonadError Error m => ScalarValue -> m ScalarValue
phase (Number y) = pure $ Number $ Cx.phase y :+ 0
phase _ = throwError expectedNumber

phase' :: MonadError Error m => Array -> m Array
phase' = scalarMonad phase

real :: MonadError Error m => ScalarValue -> m ScalarValue
real (Number y) = pure $ Number $ Cx.realPart y :+ 0
real _ = throwError expectedNumber

real' :: MonadError Error m => Array -> m Array
real' = scalarMonad real

imag :: MonadError Error m => ScalarValue -> m ScalarValue
imag (Number y) = pure $ Number $ Cx.imagPart y :+ 0
imag _ = throwError expectedNumber

imag' :: MonadError Error m => Array -> m Array
imag' = scalarMonad imag

not :: MonadError Error m => ScalarValue -> m ScalarValue
not (Number y) = pure $ Number $ 1 - y
not _ = throwError expectedNumber

not' :: MonadError Error m => Array -> m Array
not' = scalarMonad TinyAPL.Functions.not

equal :: MonadError Error m => ScalarValue -> ScalarValue -> m Bool
equal x y = pure $ x == y

equal' :: MonadError Error m => Array -> Array -> m Array
equal' = scalarDyad (fmap boolToScalar .: equal)

notEqual :: MonadError Error m => ScalarValue -> ScalarValue -> m Bool
notEqual x y = pure $ x /= y

notEqual' :: MonadError Error m => Array -> Array -> m Array
notEqual' = scalarDyad (fmap boolToScalar .: notEqual)

less :: MonadError Error m => ScalarValue -> ScalarValue -> m Bool
less x y = pure $ x < y

less' :: MonadError Error m => Array -> Array -> m Array
less' = scalarDyad (fmap boolToScalar .: less)

lessEqual :: MonadError Error m => ScalarValue -> ScalarValue -> m Bool
lessEqual x y = pure $ x <= y

lessEqual' :: MonadError Error m => Array -> Array -> m Array
lessEqual' = scalarDyad (fmap boolToScalar .: lessEqual)

greaterEqual :: MonadError Error m => ScalarValue -> ScalarValue -> m Bool
greaterEqual x y = pure $ x >= y

greaterEqual' :: MonadError Error m => Array -> Array -> m Array
greaterEqual' = scalarDyad (fmap boolToScalar .: greaterEqual)

greater :: MonadError Error m => ScalarValue -> ScalarValue -> m Bool
greater x y = pure $ x > y

greater' :: MonadError Error m => Array -> Array -> m Array
greater' = scalarDyad (fmap boolToScalar .: greater)

identical :: MonadError Error m => Array -> Array -> m Bool
identical x y = pure $ x == y

identical' :: MonadError Error m => Array -> Array -> m Array
identical' x y = scalar . boolToScalar <$> identical x y

notIdentical :: MonadError Error m => Array -> Array -> m Bool
notIdentical x y = pure $ x /= y

notIdentical' :: MonadError Error m => Array -> Array -> m Array
notIdentical' x y = scalar . boolToScalar <$> notIdentical x y

tally :: MonadError Error m => Array -> m Natural
tally y = pure $ genericLength $ majorCells y

tally' :: MonadError Error m => Array -> m Array
tally' y = scalar . Number . fromInteger . toInteger <$> tally y

nubSieve :: (Ord a, MonadError Error m) => [a] -> m [Bool]
nubSieve ys = pure $ zipWith (\c idx -> fromJust (c `elemIndex` ys) == idx) ys [0..]

nubSieve' :: MonadError Error m => Array -> m Array
nubSieve' arr = do
  nub <- nubSieve $ majorCells arr
  pure $ vector $ boolToScalar <$> nub

shape :: MonadError Error m => Array -> m [Natural]
shape y = pure $ arrayShape y

shape' :: MonadError Error m => Array -> m Array
shape' arr = do
  sh <- shape arr
  pure $ vector $ Number . fromInteger . toInteger <$> sh

reshape :: MonadError Error m => [Integer] -> Array -> m Array
reshape shape arr@(Array _ xs) = do
  let negative = count (< 0) shape
  if negative == 0 then case arrayReshaped (fromInteger . toInteger <$> shape) xs of
    Nothing -> throwError $ DomainError "Cannot reshape an empty array to a non-empty array"
    Just res -> pure res
  else if negative == 1 && -1 `elem` shape then do
    let bound = genericLength xs
    let known = product $ filter (>= 0) shape
    if known == 0 then throwError $ DomainError "Shape cannot contain both 0 and -1"
    else if bound `mod` known /= 0 then throwError $ DomainError "Shape is not a multiple of the bound of the array"
    else reshape ((\x -> if x == -1 then bound `div` known else x) <$> shape) arr
  else throwError $ DomainError "Invalid shape"

reshape' :: MonadError Error m => Array -> Array -> m Array
reshape' sh arr = do
  let err = DomainError "Shape must be an integer vector"
  shape <- asVector err sh >>= mapM (asNumber err >=> asInt err)
  reshape shape arr

rank :: MonadError Error m => Array -> m Natural
rank = pure . arrayRank

rank' :: MonadError Error m => Array -> m Array
rank' arr = scalar . Number . fromInteger . toInteger <$> rank arr

promote :: MonadError Error m => Array -> m Array
promote arr = reshape (1 : map toInteger (arrayShape arr)) arr

demote :: MonadError Error m => Array -> m Array
demote arr = case toInteger <$> arrayShape arr of
  [] -> pure arr
  [_] -> pure $ scalar $ head $ arrayContents arr
  (a:b:ss) -> reshape (a * b : ss) arr

rerank :: MonadError Error m => Natural -> Array -> m Array
rerank n arr =
  if arrayRank arr == n then pure arr
  else if arrayRank arr > n then demote arr >>= rerank n
  else promote arr >>= rerank n

rerank' :: MonadError Error m => Array -> Array -> m Array
rerank' narr arr = do
  let err = DomainError "Rerank left argument must be a scalar natural"
  n <- asScalar err narr >>= asNumber err >>= asNat err
  rerank n arr

ravel :: MonadError Error m => Array -> m [ScalarValue]
ravel arr = pure $ arrayContents arr

ravel' :: MonadError Error m => Array -> m Array
ravel' = fmap vector . ravel

enlist :: MonadError Error m => Array -> m [ScalarValue]
enlist (Array [] [Box a]) = enlist a
enlist (Array [] sc) = pure sc
enlist (Array _ cs) = concat <$> mapM (enlist . fromScalar) cs

enlist' :: MonadError Error m => Array -> m Array
enlist' y = vector <$> enlist y

depth :: MonadError Error m => Array -> m Natural
depth (Array [] [Box xs]) = (1+) <$> depth xs
depth (Array [] _) = pure 0
depth (Array _ []) = pure 1
depth (Array _ xs) = (1+) . maximum <$> mapM (depth . fromScalar) xs

depth' :: MonadError Error m => Array -> m Array
depth' = fmap (scalar . Number . fromInteger . toInteger) . depth

reverse :: MonadError Error m => [a] -> m [a]
reverse = pure . Prelude.reverse

reverse' :: MonadError Error m => Array -> m Array
reverse' = onMajorCells TinyAPL.Functions.reverse

rotate :: MonadError Error m => [Integer] -> Array -> m Array
rotate [] xs = pure xs
rotate (r:rs) xs = fromMajorCells . TinyAPL.Util.rotate r <$> mapM (TinyAPL.Functions.rotate rs) (majorCells xs)

rotate' :: MonadError Error m => Array -> Array -> m Array
rotate' rot arr = do
  let err = DomainError "Rotate left argument must be an integer vector or scalar"
  rs <- asVector err rot >>= mapM (asNumber err >=> asInt err)
  TinyAPL.Functions.rotate rs arr

take :: MonadError Error m => [Integer] -> Array -> m Array
take [] xs = pure xs
take (t:ts) xs = let
  take' c = if c < 0 then Prelude.reverse . genericTake (negate c) . Prelude.reverse else genericTake c
  in fromMajorCells . take' t <$> mapM (TinyAPL.Functions.take ts) (majorCells xs)

take' :: MonadError Error m => Array -> Array -> m Array
take' tak arr = do
  let err = DomainError "Take left argument must be an integer vector"
  ts <- asVector err tak >>= mapM (asNumber err >=> asInt err)
  TinyAPL.Functions.take ts arr

drop :: MonadError Error m => [Integer] -> Array -> m Array
drop [] xs = pure xs
drop (d:ds) xs = let
  drop' c = if c < 0 then Prelude.reverse . genericDrop (negate c) . Prelude.reverse else genericDrop c
  in fromMajorCells . drop' d <$> mapM (TinyAPL.Functions.drop ds) (majorCells xs)

drop' :: MonadError Error m => Array -> Array -> m Array
drop' dro arr = do
  let err = DomainError "Drop left argument must be an integer vector"
  ds <- asVector err dro >>= mapM (asNumber err >=> asInt err)
  TinyAPL.Functions.drop ds arr

enclose :: MonadError Error m => Array -> m ScalarValue
enclose y = pure $ box y

enclose' :: MonadError Error m => Array -> m Array
enclose' = fmap scalar . enclose

halfPair :: MonadError Error m => Array -> m Array
halfPair y = pure $ vector [box y]

pair :: MonadError Error m => Array -> Array -> m Array
pair x y = pure $ vector $ box <$> [x, y]

first :: MonadError Error m => Array -> m Array
first (Array _ []) = throwError $ DomainError "First on empty array"
first (Array _ (x:_)) = pure $ fromScalar x

last :: MonadError Error m => Array -> m Array
last (Array _ []) = throwError $ DomainError "Last on empty array"
last (Array _ xs) = pure $ fromScalar $ Prelude.last xs

indexGenerator :: MonadError Error m => Natural -> m Array
indexGenerator i = pure $ vector $ Number . fromInteger . toInteger <$> [1..i]

indexGeneratorN :: MonadError Error m => [Natural] -> m Array
indexGeneratorN is = pure $ fromJust $ arrayReshaped is $ box . fromJust . arrayReshaped [genericLength is] . fmap (Number . fromInteger . toInteger) <$> generateIndices is

indexGenerator' :: MonadError Error m => Array -> m Array
indexGenerator' arr = do
  let err = DomainError "Index Generator argument must be a natural scalar or vector"
  is <- asVector err arr >>= mapM (asNumber err >=> asNat err)
  if isScalar arr then indexGenerator $ head is
  else indexGeneratorN is

replicate :: MonadError Error m => [Natural] -> [a] -> m [a]
replicate [] [] = pure []
replicate (r:rs) (x:xs) = (genericReplicate r x ++) <$> TinyAPL.Functions.replicate rs xs
replicate _ _ = throwError $ LengthError "Replicate with different lengths"

replicate' :: MonadError Error m => Array -> Array -> m Array
replicate' rs arr = do
  let err = DomainError "Replicate left argument must be a natural vector or scalar"
  rs' <- asVector err rs >>= mapM (asNumber err >=> asNat err)
  let cells = majorCells arr
  let rs'' = if isScalar rs then Prelude.replicate (length cells) (head rs') else rs'
  fromMajorCells <$> TinyAPL.Functions.replicate rs'' cells

indices :: MonadError Error m => Array -> m Array
indices (Array sh cs) = do
  let err = DomainError "Indices argument must be an array of naturals"
  let indices = generateIndices sh
  let shape = [genericLength sh | length sh /= 1]
  let rep idx c = genericReplicate c $ box $ fromJust $ arrayReshaped shape $ Number . fromInteger . toInteger <$> idx
  counts <- mapM (asNumber err >=> asNat err) cs
  pure $ vector $ concat $ zipWith rep indices counts

unique :: (Eq a, MonadError Error m) => [a] -> m [a]
unique xs = pure $ nub xs

unique' :: MonadError Error m => Array -> m Array
unique' arr = fromMajorCells <$> unique (majorCells arr)

union :: (Eq a, MonadError Error m) => [a] -> [a] -> m [a]
union xs ys = pure $ xs ++ filter (Prelude.not . (`elem` xs)) ys

union' :: MonadError Error m => Array -> Array -> m Array
union' x y = fromMajorCells <$> union (majorCells x) (majorCells y)

intersection :: (Eq a, MonadError Error m) => [a] -> [a] -> m [a]
intersection xs ys = pure $ filter (`elem` ys) xs

intersection' :: MonadError Error m => Array -> Array -> m Array
intersection' x y = fromMajorCells <$> intersection (majorCells x) (majorCells y)

difference :: (Eq a, MonadError Error m) => [a] -> [a] -> m [a]
difference xs ys = pure $ filter (Prelude.not . (`elem` ys)) xs

difference' :: MonadError Error m => Array -> Array -> m Array
difference' x y = fromMajorCells <$> difference (majorCells x) (majorCells y)

symmetricDifference :: (Eq a, MonadError Error m) => [a] -> [a] -> m [a]
symmetricDifference xs ys = do
  a <- difference xs ys
  b <- difference ys xs
  pure $ a ++ b

symmetricDifference' :: MonadError Error m => Array -> Array -> m Array
symmetricDifference' x y = fromMajorCells <$> symmetricDifference (majorCells x) (majorCells y)

roll :: (MonadError Error m, MonadIO m) => Natural -> m Double
roll y =
  if y == 0 then randomR (0, 1)
  else fromInteger <$> randomR (1, toInteger y)

roll' :: (MonadError Error m, MonadIO m) => Array -> m Array
roll' = scalarMonad $ \y -> do
  n <- asNumber expectedNatural y >>= asNat expectedNatural
  Number . (:+ 0) <$> roll n

indexCells :: MonadError Error m => [Integer] -> [a] -> m [a]
indexCells [] _ = pure []
indexCells (i:is) xs
  | i < 0 = indexCells (genericLength xs + i + 1 : is) xs
  | i == 0 || i > genericLength xs = throwError $ DomainError "Index out of bounds"
  | otherwise = (genericIndex xs (i - 1) :) <$> indexCells is xs

indexDeep :: MonadError Error m => [[Integer]] -> Array -> m Array
indexDeep [] arr = pure arr
indexDeep (i:is) arr = indexCells i (majorCells arr) >>= (fmap (\x -> if length x == 1 then head x else fromMajorCells x) . mapM (indexDeep is))

indexScatter :: MonadError Error m => Array -> Array -> m Array
indexScatter iarr carr = each1 (\i -> do
  let err = DomainError "From left argument must be an integer vector or array of integer vectors"
  is <- asVector err i >>= mapM (asNumber err >=> asInt err)
  indexDeep (singleton <$> is) carr) iarr

from :: MonadError Error m => Array -> Array -> m Array
from iarr carr
  | null (arrayShape iarr) && all (\case { Number _ -> True; _ -> False }) (arrayContents iarr) = do
    let err = DomainError "From left argument must be an integer vector or an array of integer vectors"
    index <- asScalar err iarr >>= asNumber err >>= asInt err
    head <$> indexCells [index] (majorCells carr)
  | length (arrayShape iarr) == 1 && all (\case { Number _ -> True; _ -> False }) (arrayContents iarr) = do
    let err = DomainError "From left argument must be an integer vector or an array of integer vectors"
    indices <- asVector err iarr >>= mapM (asNumber err >=> asInt err)
    fromMajorCells <$> indexCells indices (majorCells carr)
  | otherwise = indexScatter iarr carr

squad :: MonadError Error m => Array -> Array -> m Array
squad iarr carr = do
  let err = DomainError "Squad left argument must be a vector of scalar integers or vectors of integers"
  is <- asVector err iarr >>= mapM (asVector err >=> mapM (asNumber err >=> asInt err)) . fmap fromScalar
  indexDeep is carr

-- * Operators

compose :: MonadError Error m => (b -> m c) -> (a -> m b) -> a -> m c
compose f g = g >=> f

reverseCompose :: MonadError Error m => (a -> m b) -> (b -> m c) -> a -> m c
reverseCompose = flip compose

atop :: MonadError Error m => (c -> m d) -> (a -> b -> m c) -> a -> b -> m d
atop f g x y = g x y >>= f

over :: MonadError Error m => (b -> b -> m c) -> (a -> m b) -> a -> a -> m c
over f g x y = do
  x' <- g x
  y' <- g y
  f x' y'

after :: MonadError Error m => (a -> c -> m d) -> (b -> m c) -> a -> b -> m d
after f g x y = do
  y' <- g y
  f x y'

before :: MonadError Error m => (a -> m b) -> (b -> c -> m d) -> a -> c -> m d
before f g x y = do
  x' <- f x
  g x' y

leftHook :: MonadError Error m => (a -> m b) -> (b -> a -> m c) -> a -> m c
leftHook f g y = do
  y' <- f y
  g y' y

rightHook :: MonadError Error m => (a -> b -> m c) -> (a -> m b) -> a -> m c
rightHook f g y = do
  y' <- g y
  f y y'

constant1 :: MonadError Error m => a -> b -> m a
constant1 a _ = pure a

constant2 :: MonadError Error m => a -> b -> c -> m a
constant2 a _ _ = pure a

duplicate :: MonadError Error m => (a -> a -> m b) -> a -> m b
duplicate f y = f y y

commute :: MonadError Error m => (b -> a -> m c) -> a -> b -> m c
commute f x y = f y x

reduce :: MonadError Error m => (a -> a -> m a) -> [a] -> m a
reduce _ [] = throwError $ DomainError "Reduce empty axis"
reduce _ [x] = pure x
reduce f (a:b:xs) = do
  x <- f a b
  reduce f $ x : xs

reduce' :: MonadError Error m => (Array -> Array -> m Array) -> Array -> m Array
reduce' f = reduce f . majorCells

reduceBack :: MonadError Error m => (a -> a -> m a) -> [a] -> m a
reduceBack _ [] = throwError $ DomainError "Reduce empty axis"
reduceBack _ [x] = pure x
reduceBack f (x:xs) = reduceBack f xs >>= f x

reduceBack' :: MonadError Error m => (Array -> Array -> m Array) -> Array -> m Array
reduceBack' f = reduceBack f . majorCells

onPrefixes :: MonadError Error m => ([a] -> m b) -> [a] -> m [b]
onPrefixes f = mapM f . prefixes

onPrefixes' :: MonadError Error m => (Array -> m Array) -> Array -> m Array
onPrefixes' f arr = fromMajorCells <$> onPrefixes (f . fromMajorCells) (majorCells arr)

onSuffixes :: MonadError Error m => ([a] -> m b) -> [a] -> m [b]
onSuffixes f = mapM f . suffixes

onSuffixes' :: MonadError Error m => (Array -> m Array) -> Array -> m Array
onSuffixes' f arr = fromMajorCells <$> onSuffixes (f . fromMajorCells) (majorCells arr)

each1 :: MonadError Error m => (Array -> m Array) -> Array -> m Array
each1 f (Array sh cs) = Array sh <$> mapM (fmap box . f . fromScalar) cs

each2 :: MonadError Error m => (Array -> Array -> m Array) -> Array -> Array -> m Array
each2 f (Array ash acs) (Array bsh bcs)
  | null ash && null bsh = scalar . box <$> f (fromScalar $ head acs) (fromScalar $ head bcs)
  | null ash = Array bsh <$> mapM (fmap box . ((fromScalar $ head acs) `f`) . fromScalar) bcs
  | null bsh = Array ash <$> mapM (fmap box . (`f` (fromScalar $ head bcs)) . fromScalar) acs
  | ash == bsh = Array ash <$> zipWithM (fmap box .: f) (fromScalar <$> acs) (fromScalar <$> bcs)
  | length ash /= length bsh = throwError $ RankError "Incompatible ranks to Each"
  | otherwise = throwError $ LengthError "Incompatible shapes to Each"

eachLeft :: MonadError Error m => (Array -> Array -> m Array) -> Array -> Array -> m Array
eachLeft f x y = each2 f x (scalar $ box y)

eachRight :: MonadError Error m => (Array -> Array -> m Array) -> Array -> Array -> m Array
eachRight f x = each2 f (scalar $ box x)

key :: (Eq a, MonadError Error m) => (a -> [b] -> m c) -> [a] -> [b] -> m [c]
key f ks vs
  | length ks == length vs = mapM (uncurry f) (group ks vs)
  | otherwise = throwError $ LengthError "Incompatible shapes to Key"

key' :: MonadError Error m => (Array -> Array -> m Array) -> Array -> Array -> m Array
key' f karr varr = fromMajorCells <$> key (\k vs -> f k $ vector $ toScalar <$> vs) (majorCells karr) (majorCells varr)

keyMonad :: MonadError Error m => (Array -> Array -> m Array) -> Array -> m Array
keyMonad f arr = do
  t <- tally' arr
  is <- indexGenerator' t
  key' f arr is

parseRank :: MonadError Error m => Array -> m (Integer, Integer, Integer)
parseRank arr = do
  let err = DomainError "Rank or depth right operand must be a 1-, 2- or 3-element integer vector"
  v <- asVector err arr >>= mapM (asNumber err >=> asInt err)
  case v of
    [d] -> pure (d, d, d)
    [d, e] -> pure (e, d, e)
    [d, e, f] -> pure (d, e, f)
    _ -> throwError err

atRank1 :: MonadError Error m => (Array -> m Array) -> Integer -> Array -> m Array
atRank1 f rank arr
  | arrayRank arr == 0 = f arr
  | rank >= 0 && toInteger (arrayRank arr) <= rank = f arr
  | rank >= 0 = fromMajorCells <$> mapM (atRank1 f rank) (majorCells arr)
  | rank == -1 = fromMajorCells <$> mapM f (majorCells arr)
  | otherwise = fromMajorCells <$> mapM (atRank1 f $ rank + 1) (majorCells arr)

atRank2 :: MonadError Error m => (Array -> Array -> m Array) -> (Integer, Integer) -> Array -> Array -> m Array
atRank2 f (ra, rb) a b = do
  -- @dzaima (in fact, {a b c←⌽3⍴⌽⍵⍵ ⋄ ↑(⊂⍤b⊢⍺) ⍺⍺¨ ⊂⍤c⊢⍵} is an impl of the dyadic case from the monadic one (with Dyalog's ↑ meaning))
  as <- atRank1 enclose' ra a
  bs <- atRank1 enclose' rb b
  each2 f as bs >>= atRank1 first 0

atRank1' :: MonadError Error m => (Array -> m Array) -> Array -> Array -> m Array
atRank1' f r y = do
  (a, _, _) <- parseRank r
  atRank1 f a y

atRank2' :: MonadError Error m => (Array -> Array -> m Array) -> Array -> Array -> Array -> m Array
atRank2' f r x y = do
  (_, b, c) <- parseRank r
  atRank2 f (b, c) x y
