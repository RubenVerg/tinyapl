{-# LANGUAGE LambdaCase #-}

module TinyAPL.Util where
import TinyAPL.Complex
import qualified TinyAPL.Glyphs as G

import GHC.Float (floatToDigits)
import GHC.Float.RealFracMethods (truncateDoubleInteger)
import Data.Char (intToDigit)
import Data.List (genericLength, genericIndex)

infixr 9 .:
(.:) f g x y = f $ g x y

showAplDouble :: Double -> String
showAplDouble x 
  | isNaN x = "<NaN>"
  | isInfinite x = if x > 0 then [G.infinity] else [G.negative, G.infinity]
  | floor x == ceiling x = let
    isNegative = x < 0
    pos = show $ truncateDoubleInteger $ abs x
    in if isNegative then G.negative : pos else pos
  | otherwise = let
    isNegative = x < 0
    x' = abs x
    (is, e) = floatToDigits 10 x'
    ds = intToDigit <$> is
    pos = if e < 0 || e > 7 then
      let
        e' = e - 1
        show_e' = (if e' < 0 then [G.exponent, G.negative] else [G.exponent]) ++ show (abs e')
      in case ds of
        []      -> "0"
        "0"     -> "0"
        [d]     -> d : show_e'
        (d:ds') -> d : '.' : ds' ++ show_e'
    else let
      mk0 "" = "0"
      mk0 xs = xs
      f 0 s     rs = mk0 (reverse s) ++ '.' : mk0 rs
      f n s     "" = f (n - 1) ('0' : s) ""
      f n s (r:rs) = f (n - 1) (r : s) rs
      in f e "" ds
    in if isNegative then G.negative : pos else pos

count :: Num n => (a -> Bool) -> [a] -> n
count _ [] = 0
count p (x:xs) | p x       = 1 + count p xs
               | otherwise = count p xs

generateIndices :: (Enum a, Num a) => [a] -> [[a]]
generateIndices = foldr (liftA2 (:) . enumFromTo 1) [[]]

mapAdjacent :: (a -> a -> b) -> [a] -> [b]
mapAdjacent f xs = zipWith f xs $ drop 1 xs

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update k v [] = [(k, v)]
update k v (x@(k', _) : xs) | k == k' = (k, v) : xs
                            | otherwise = x : update k v xs

delete :: Eq a => [(a, b)] -> a -> [(a, b)]
delete xs k = filter ((k /=) . fst) xs

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just x) = Right x
maybeToEither x Nothing = Left x

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes (x:xs) = [x] : ((x :) <$> prefixes xs)

suffixes :: [a] -> [[a]]
suffixes = reverse . map reverse . prefixes . reverse

i :: Num a => Complex a
i = 0 :+ 1

rotate :: (Ord n, Num n) => n -> [a] -> [a]
rotate c 
  | c < 0 = reverse . rotate (negate c) . reverse
  | c == 0 = id
  | otherwise = \case
    [] -> []
    (x:xs) -> rotate (c - 1) (xs ++ [x])

componentFloor :: RealFrac a => Complex a -> Complex a
componentFloor (r :+ i) = fromInteger (floor r) :+ fromInteger (floor i)

fracPart :: RealFrac a => a -> a
fracPart = snd . properFraction . (1 +) . snd . properFraction -- properFraction returns a negative number for negative inputs, 1| doesn't

-- https://aplwiki.com/wiki/Complex_floor
complexFloor :: RealFloat a => Complex a -> Complex a
complexFloor (r :+ i) = let
  b = componentFloor $ r :+ i
  x = fracPart r
  y = fracPart i
  in
    if x + y < 1 then b
    else if x >= y then b + 1
    else b + (0 :+ 1)

complexCeiling :: RealFloat a => Complex a -> Complex a
complexCeiling = negate . complexFloor . negate

complexRemainder :: RealFloat a => Complex a -> Complex a -> Complex a
complexRemainder w z = z - w * complexFloor (if w == 0 then z else z / w)

complexGCD :: RealFloat a => Complex a -> Complex a -> Complex a
complexGCD a w = if a `complexRemainder` w == 0 then a else (a `complexRemainder` w) `complexGCD` a

complexLCM :: RealFloat a => Complex a -> Complex a -> Complex a
complexLCM x y = if x == 0 && y == 0 then 0 else (x * y) / (x `complexGCD` y)

group :: Eq k => [k] -> [a] -> [(k, [a])]
group [] [] = []
group (k:ks) (a:as) = let gs = group ks as in case lookup k gs of
  Just g -> (k, a:g) : delete gs k
  Nothing -> (k, [a]) : gs
group _ _ = error "group: mismatched array lengths"

groupBy :: Eq k => (a -> k) -> [a] -> [(k, [a])]
groupBy f as = group (f <$> as) as

oneIndex :: Integral n => [a] -> [n] -> Maybe [a]
oneIndex _ [] = Just []
oneIndex as (i:is)
  | i == 0 = Nothing
  | i < 0 = oneIndex as $ (1 + i + genericLength as) : is
  | i > genericLength as = Nothing
  | otherwise = ((as `genericIndex` (i - 1)) :) <$> oneIndex as is

setAt :: Integral n => n -> a -> [a] -> [a]
setAt 0 r [] = [r]
setAt _ _ [] = error "setAt: out of bounds"
setAt 0 r (_:xs) = r : xs
setAt n r (x:xs) = x : setAt (n - 1) r xs

whileM :: Monad m => m Bool -> m a -> m [a]
whileM p f = go where
  go = do
    x <- p
    if x then do
      r <- f
      rs <- go
      pure $ r : rs
    else pure []

whileM_ :: Monad m => m Bool -> m a -> m ()
whileM_ p f = go where
  go = do
    x <- p
    if x then f >> go else pure ()

distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = x `notElem` xs && distinct xs
