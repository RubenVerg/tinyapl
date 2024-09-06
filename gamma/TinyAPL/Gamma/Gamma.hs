{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-all #-}

module TinyAPL.Gamma.Gamma
  ( Gamma(..)
  , Factorial(..)
  , IncGamma(..)
  , GenGamma(..) ) where

import TinyAPL.Gamma.Gamma.Lanczos
import TinyAPL.Gamma.Gamma.Incomplete
import TinyAPL.Gamma.Factorial

import TinyAPL.Complex (Complex (..))
import Data.List (findIndex)
import GHC.Float (float2Double, double2Float)
import qualified Data.Vector.Unboxed as V
import Math.ContinuedFraction (modifiedLentz)
import Math.Sequence.Converge (converge)
import Data.Maybe (fromJust)

-- |Gamma function.  Minimal definition is ether 'gamma' or 'lnGamma'.
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

floatGammaInfCutoff :: Double
floatGammaInfCutoff = fromJust $ (1 +) . toEnum <$> findIndex isInfinite (scanl (*) (1::Float) [1..])

instance Gamma Float where
  gamma = double2Float . gam . float2Double
    where
      gam x 
        | x >= floatGammaInfCutoff  = 1/0
        | otherwise = case properFraction x of
        (n,0) | n < 1     -> 0/0
              | otherwise -> factorial (n-1 :: Integer)
        _     | x < (-20) -> let s = pi / sin (pi * x)
                              in signum s * exp (log (abs s) - lnGamma (1-x))
              | otherwise -> reflect (gammaLanczos g cs) x
        
      g = pi
      cs = [ 1.0000000249904433
           , 9.100643759042066
           ,-4.3325519094475
           , 0.12502459858901147
           , 1.1378929685052916e-4
           ,-9.555011214455924e-5 ]
  
  lnGamma = double2Float . reflectLn (lnGammaLanczos g cs) . float2Double
    where
      g = pi
      cs = [ 1.0000000249904433
           , 9.100643759042066
           ,-4.3325519094475
           , 0.12502459858901147
           , 1.1378929685052916e-4
           ,-9.555011214455924e-5 ]
  
  lnFactorial n
    | n' < 0                = error "lnFactorial n: n < 0"
    | n' < toInteger nFacs  = facs V.! fromIntegral n
    | otherwise             = lnGamma (fromIntegral n+1)
    where
      n' = toInteger n
      nFacs       = 2000 -- limited only by time and space
      facs        = V.map lnGamma (V.enumFromN 1 nFacs)

doubleGammaInfCutoff :: Double
doubleGammaInfCutoff = fromJust $ (1 +) . toEnum <$> findIndex isInfinite (scanl (*) (1::Double) [1..])

instance Gamma Double where
  gamma x 
    | x >= doubleGammaInfCutoff  = 1/0
    | otherwise = case properFraction x of
    (n,0) | n < 1     -> 0/0
          | otherwise -> factorial (n-1 :: Integer)
    _     | x < (-50) -> let s = pi / sin (pi * x)
                          in signum s * exp (log (abs s) - lnGammaLanczos g cs (1-x))
          | otherwise -> reflect (gammaLanczos g cs) x
    where
      g = 2*pi
      cs = [   0.9999999999999858
           , 311.6011750541472
           ,-498.6511904603639
           , 244.08472899976877
           , -38.670364643074194
           ,   1.3350900101370549
           ,  -1.8977221899565682e-3
           ,   8.475264614349149e-7
           ,   2.59715567376858e-7
           ,  -2.7166437850607517e-7
           ,   6.151114806136299e-8 ]

  lnGamma = reflectLn (lnGammaLanczos g cs)
    where
      g = exp pi / pi
      cs = [    1.0000000000000002
           , 1002.5049417114732
           ,-1999.6140446432912
           , 1352.1626218340114
           , -360.6486475548049
           ,   33.344988357090685
           ,    -0.6637188712004668
           ,     5.16644552377916e-4
           ,     1.684651140163429e-7
           ,    -1.8148207145896904e-7
           ,     6.171532716135051e-8
           ,    -9.014004881476154e-9 ]

  lnFactorial n
    | n' < 0                = error "lnFactorial n: n < 0"
    | n' < toInteger nFacs  = facs V.! fromIntegral n
    | otherwise             = lnGamma (fromIntegral n+1)
    where
      n' = toInteger n
      nFacs = 2000 -- limited only by time and space
      facs = V.map lnGamma (V.enumFromN 1 nFacs)

complexDoubleToFloat :: Complex Double -> Complex Float
complexDoubleToFloat (a :+ b) = double2Float a :+ double2Float b
complexFloatToDouble :: Complex Float -> Complex Double
complexFloatToDouble (a :+ b) = float2Double a :+ float2Double b

instance Gamma (Complex Float) where
  gamma = complexDoubleToFloat . gamma . complexFloatToDouble
  lnGamma = complexDoubleToFloat . reflectLnC (lnGammaLanczos g cs) . complexFloatToDouble
    where
      g = pi
      cs = [ 1.0000000249904433
           , 9.100643759042066
           ,-4.3325519094475
           , 0.12502459858901147
           , 1.1378929685052916e-4
           ,-9.555011214455924e-5 ]

  
  lnFactorial n
    | n' < 0                = error "lnFactorial n: n < 0"
    | n' < toInteger nFacs  = facs V.! fromIntegral n
    | otherwise             = lnGamma (fromIntegral n+1)
    where
        n' = toInteger n
        nFacs = 2000 -- limited only by time and space
        facs = V.map lnGamma (V.enumFromN 1 nFacs)

instance Gamma (Complex Double) where
  gamma = reflectC (gammaLanczos g cs)
    where
      g = 2*pi
      cs = [   1.0000000000000002
           , 311.60117505414695
           ,-498.65119046033163
           , 244.08472899875767
           , -38.67036462939322
           ,   1.3350899103585203
           ,  -1.8972831806242229e-3
           ,  -3.935368195357295e-7
           ,   2.592464641764731e-6
           ,  -3.2263565156368265e-6
           ,   2.5666169886566876e-6
           ,  -1.3737776806198937e-6
           ,   4.4551204024819644e-7
           ,  -6.576826592057796e-8 ]

  lnGamma = reflectLnC (lnGammaLanczos g cs)
    where
      g = exp pi / pi
      cs = [    1.0000000000000002
           , 1002.5049417114732
           ,-1999.6140446432912
           , 1352.1626218340114
           , -360.6486475548049
           ,   33.344988357090685
           ,    -0.6637188712004668
           ,     5.16644552377916e-4
           ,     1.684651140163429e-7
           ,    -1.8148207145896904e-7
           ,     6.171532716135051e-8
           ,    -9.014004881476154e-9 ]

  lnFactorial n
    | n' < 0                = error "lnFactorial n: n < 0"
    | n' < toInteger nFacs  = facs V.! fromIntegral n
    | otherwise             = lnGamma (fromIntegral n+1)
    where
      n' = toInteger n
      nFacs = 2000 -- limited only by time and space
      facs = V.map lnGamma (V.enumFromN 1 nFacs)


-- |Incomplete gamma functions.
class Gamma a => IncGamma a where
  -- |Lower gamma function: lowerGamma s x == integral from 0 to x of 
  -- @\t -> t**(s-1) * exp (negate t)@
  lowerGamma :: a -> a -> a
  -- |Natural log of lower gamma function
  lnLowerGamma :: a -> a -> a 
  -- |Regularized lower incomplete gamma function: lowerGamma s x / gamma s
  p :: a -> a -> a
  
  -- |Upper gamma function: lowerGamma s x == integral from x to infinity of 
  -- @\t -> t**(s-1) * exp (negate t)@
  upperGamma :: a -> a -> a
  -- |Natural log of upper gamma function
  lnUpperGamma :: a -> a -> a
  -- |Regularized upper incomplete gamma function: upperGamma s x / gamma s
  q :: a -> a -> a

-- |This instance uses the Double instance.
instance IncGamma Float where
  lowerGamma   s x = double2Float $ (lowerGamma   :: Double -> Double -> Double) (float2Double s) (float2Double x)
  lnLowerGamma s x = double2Float $ (lnLowerGamma :: Double -> Double -> Double) (float2Double s) (float2Double x)
  p s x = double2Float $ (p :: Double -> Double -> Double) (float2Double s) (float2Double x)
  
  upperGamma   s x = double2Float $ (upperGamma   :: Double -> Double -> Double) (float2Double s) (float2Double x)
  lnUpperGamma s x = double2Float $ (lnUpperGamma :: Double -> Double -> Double) (float2Double s) (float2Double x)
  q s x = double2Float $ (q :: Double -> Double -> Double) (float2Double s) (float2Double x)

-- |I have not yet come up with a good strategy for evaluating these 
-- functions for negative @x@.  They can be rather numerically unstable.
instance IncGamma Double where
  lowerGamma s x
    | x < 0     = error "lowerGamma: x < 0 is not currently supported."
    | x == 0    = 0
    | x >= s+1  = gamma s - upperGamma s x
    | otherwise = lowerGammaHypGeom s x

  upperGamma s x
    | x < 0     = error "upperGamma: x < 0 is not currently supported."
    | x == 0    = gamma s
    | x < s+1   = q s x * gamma s
    | otherwise = converge . concat
      $ modifiedLentz 1e-30 (upperGammaCF s x)
  
  lnLowerGamma s x
    | x < 0     = error "lnLowerGamma: x < 0 is not currently supported."
    | x == 0    = log 0
    | x >= s+1  = log (p s x) + lnGamma s
    | otherwise = lnLowerGammaHypGeom s x
  
  lnUpperGamma s x
    | x < 0     = error "lnUpperGamma: x < 0 is not currently supported."
    | x == 0    = lnGamma s
    | x < s+1   = log (q s x) + lnGamma s
    | otherwise =
      converge (lnUpperGammaConvergents s x)
  
  p s x
    | x < 0     = error "p: x < 0 is not currently supported."
    | x == 0    = 0
    | x >= s+1  = 1 - q s x
    | otherwise = pHypGeom s x
  
  q s x
    | x < 0     = error "q: x < 0 is not currently supported."
    | x == 0    = 1
    | x < s+1   = 1 - p s x
    | otherwise =
      converge . concat
      $ modifiedLentz 1e-30 (qCF s x)

-- |Generalized gamma function (also known as multivariate gamma function).
-- See http://en.wikipedia.org/wiki/Multivariate_gamma_function
-- (Fetched Feb 29, 2012).
class Gamma a => GenGamma a where
  -- |Generalized gamma function.  generalizedGamma p x = (pi ** ((p - 1) / 2)) * gamma x * generalizedGamma (p - 1) (x - 0.5)
  generalizedGamma :: Int -> a -> a
  -- |Natural log of generalizedGamma
  lnGeneralizedGamma :: Int -> a -> a

instance GenGamma Float where
  generalizedGamma   u x = double2Float $ (generalizedGamma   :: Int -> Double -> Double) u (float2Double x)
  lnGeneralizedGamma u x = double2Float $ (lnGeneralizedGamma :: Int -> Double -> Double) u (float2Double x)

instance GenGamma Double where
  generalizedGamma u x
    | u <= 0    = error "generalizedGamma p x: p must be strictly positive."
    | u == 1    = gamma x
    | otherwise = pi ** (0.25 * u' * (u' - 1))
        * product [ gamma (x - 0.5 * fromIntegral j) | j <- [0 .. u - 1]]
    where u' = fromIntegral u

  lnGeneralizedGamma u x
    | u <= 0    = error "lnGeneralizedGamma p x: p must be strictly positive."
    | u == 1    = lnGamma x
    | otherwise = (0.25 * log pi) * (u' * (u' - 1))
        + sum [ lnGamma (x - 0.5 * fromIntegral j) | j <- [0 .. u - 1]]
    where u' = fromIntegral u