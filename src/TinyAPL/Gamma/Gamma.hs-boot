module TinyAPL.Gamma.Gamma where

import TinyAPL.Gamma.Factorial

class (Eq a, Floating a, Factorial a) => Gamma a where
  -- |The gamma function:  gamma z == integral from 0 to infinity of
  -- @\t -> t**(z-1) * exp (negate t)@
  gamma :: a -> a
  gamma 0 = 0/0
  gamma z
      | z == abs z    = exp (lnGamma z)
      | otherwise     = pi / (sin (pi * z) * exp (lnGamma (1 - z)))


  -- |Natural log of the gamma function
  lnGamma :: a -> a
  lnGamma z = log (gamma z)
  
  -- |Natural log of the factorial function
  lnFactorial :: Integral b => b -> a
  lnFactorial n = lnGamma (fromIntegral n + 1)

instance Gamma Float where
instance Gamma Double where