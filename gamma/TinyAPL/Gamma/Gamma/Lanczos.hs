{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wno-all #-}

-- |Lanczos' approximation to the gamma function, as described at
-- http:\/\/en.wikipedia.org\/wiki\/Lanczos_approximation
-- (fetched 11 June 2010).
-- 
-- Constants to be supplied by user.  There is a file \"extras/LanczosConstants.hs\"
-- in the source repository that implements a technique by Paul Godfrey for
-- calculating the coefficients.  It is not included in the distribution yet 
-- because it makes use of a linear algebra library I have not yet released 
-- (though I eventually intend to).
module TinyAPL.Gamma.Gamma.Lanczos
  ( gammaLanczos, lnGammaLanczos
  , reflect, reflectC
  , reflectLn, reflectLnC ) where

import TinyAPL.Complex (Complex, imagPart, realPart)

-- |Compute Lanczos' approximation to the gamma function, using the specified
-- constants.  Valid for Re(x) > 0.5.  Use 'reflect' or 'reflectC' to extend
-- to the whole real line or complex plane, respectively.
{-# INLINE gammaLanczos #-}
gammaLanczos :: Floating a => a -> [a] -> a -> a
gammaLanczos _ [] _ = error "gammaLanczos: empty coefficient list"
gammaLanczos g cs zp1
  = sqrt (2*pi) * x ** (zp1 - 0.5) * exp (negate x) * a cs z
  where
    x = zp1 + (g - 0.5)
    z = zp1 - 1

-- |Compute Lanczos' approximation to the natural logarithm of the gamma
-- function, using the specified constants.  Valid for Re(x) > 0.5.  Use
-- 'reflectLn' or 'reflectLnC' to extend to the whole real line or complex
-- plane, respectively.
{-# INLINE lnGammaLanczos #-}
lnGammaLanczos :: Floating a => a -> [a] -> a -> a
lnGammaLanczos _ [] _ = error "lnGammaLanczos: empty coefficient list"
lnGammaLanczos g cs zp1 
  = log (sqrt (2*pi)) + log x * (zp1 - 0.5) - x + log (a cs z)
  where 
    x = zp1 + (g - 0.5)
    z = zp1 - 1

{-# INLINE a #-}
a :: Fractional t => [t] -> t -> t
a [] _ = error "Math.Gamma.Lanczos.a: empty coefficient list"
a cs z = head cs + sum [c / (z + k) | c <- tail cs | k <- iterate (1+) 1]

fractionalPart :: RealFloat a => a -> a
fractionalPart x = case properFraction x of
  (i,f) -> let _ = i :: Int in f

-- |Extend an approximation of the gamma function from the domain x > 0.5 to
-- the whole real line.
{-# INLINE reflect #-}
reflect :: (RealFloat a, Ord a) => (a -> a) -> a -> a
reflect gamma z
  | z > 0.5               = gamma z
  | fractionalPart z == 0 = 0
  | otherwise             = pi / (sin (pi * z) * gamma (1-z))

-- |Extend an approximation of the gamma function from the domain Re(x) > 0.5
-- to the whole complex plane.
{-# INLINE reflectC #-}
reflectC :: RealFloat a => (Complex a -> Complex a) -> Complex a -> Complex a
reflectC gamma z
  | realPart z > 0.5  = gamma z
  | imagPart z == 0 && fractionalPart (realPart z) == 0 = 0/0
  | otherwise         = pi / (sin (pi * z) * gamma (1-z))

-- |Extend an approximation of the natural logarithm of the gamma function 
-- from the domain x > 0.5 to the whole real line.
{-# INLINE reflectLn #-}
reflectLn :: (RealFloat a, Ord a) => (a -> a) -> a -> a
reflectLn lnGamma z
  | z > 0.5               = lnGamma z
  | fractionalPart z == 0 = log (0/0)
  | otherwise             = log pi - log (sin (pi * z)) - lnGamma (1-z)

-- |Extend an approximation of the natural logarithm of the gamma function 
-- from the domain Re(x) > 0.5 to the whole complex plane.
{-# INLINE reflectLnC #-}
reflectLnC :: RealFloat a => (Complex a -> Complex a) -> Complex a -> Complex a
reflectLnC lnGamma z
  | realPart z > 0.5  = lnGamma z
  | imagPart z == 0 && fractionalPart (realPart z) == 0 = log (0/0)
  | otherwise = log pi - log (sin (pi * z)) - lnGamma (1-z)