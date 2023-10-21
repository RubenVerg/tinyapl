module TinyAPL.Util where
import qualified TinyAPL.Glyphs as G
import GHC.Float (floatToDigits, isInfinite)
import GHC.Float.RealFracMethods (truncateDoubleInteger)
import Data.Char (intToDigit)

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