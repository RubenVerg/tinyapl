{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wno-all #-}

module TinyAPL.Gamma.Gamma.Incomplete
  ( lowerGammaCF, pCF
  , lowerGammaHypGeom, lnLowerGammaHypGeom, pHypGeom
  , upperGammaCF, lnUpperGammaConvergents, qCF ) where

import {-# SOURCE #-} TinyAPL.Gamma.Gamma

import Math.ContinuedFraction    (CF, gcf, modifiedLentzWith, sumPartialProducts)
import Math.Sequence.Converge    (converge)

-- |Continued fraction representation of the lower incomplete gamma function.
lowerGammaCF :: (Floating a, Ord a) => a -> a -> Math.ContinuedFraction.CF a
lowerGammaCF s z = gcf 0
  [ (p,q)
  | p <- pow_x_s_div_exp_x s z
    : interleave
      [negate spn * z | spn <- iterate (1+) s]
      [n * z          | n   <- iterate (1+) 1]
  | q <- iterate (1+) s
  ]

-- |Lower incomplete gamma function, computed using Kummer's confluent
-- hypergeometric function M(a;b;x).  Specifically, this uses the identity:
-- 
-- gamma(s,x) = x**s * exp (-x) / s * M(1; 1+s; x)
-- 
-- From Abramowitz & Stegun (6.5.12).
--
-- Recommended for use when x < s+1
lowerGammaHypGeom :: (Eq b, Floating b) => b -> b -> b
lowerGammaHypGeom 0 0 = 0/0
lowerGammaHypGeom s x = x ** s * exp (negate x) / s * m_1_sp1 s x

-- |Natural logarithm of lower gamma function, based on the same identity as
-- 'lowerGammaHypGeom' and evaluated carefully to avoid overflow and underflow.
-- Recommended for use when x < s+1
lnLowerGammaHypGeom :: (Eq a, Floating a) => a -> a -> a
lnLowerGammaHypGeom 0 0 = 0/0
lnLowerGammaHypGeom s x 
  = log ((signum x)**s * sign_m / signum s)
  + s*log (abs x) - x - log (abs s) + log_m
  where
      (sign_m, log_m) = log_m_1_sp1 s x

-- |Continued fraction representation of the regularized lower incomplete gamma function.
pCF :: (Gamma a, Ord a, Enum a) => a -> a -> CF a
pCF s x = gcf 0
  [ (p,q)
  | p <- pow_x_s_div_gamma_s_div_exp_x s x
    : interleave
      [negate spn * x | spn <- [s..]]
      [n * x          | n   <- [1..]]
  | q <- [s..]
  ]

-- |Regularized lower incomplete gamma function, computed using Kummer's
-- confluent hypergeometric function.  Uses same identity as 'lowerGammaHypGeom'.
-- 
-- Recommended for use when x < s+1
pHypGeom :: (Gamma a, Ord a) => a -> a -> a
pHypGeom 0 0 = 0/0
pHypGeom s x
  | s < 0
  = signum x ** s * sin (pi*s) / (-pi)
  * exp (s * log (abs x) - x + lnGamma  (-s)) * m_1_sp1 s x

  | s == 0 || x == 0
  = 0

  | otherwise
  = signum x ** s * exp (s * log (abs x) - x - lnGamma (s+1)) * m_1_sp1 s x

-- |Continued fraction representation of the regularized upper incomplete gamma function.
-- Recommended for use when x >= s+1
qCF :: (Gamma a, Ord a, Enum a) => a -> a -> CF a
qCF s x = gcf 0
  [ (p,q)
  | p <- pow_x_s_div_gamma_s_div_exp_x s x
    : zipWith (*) [1..] (iterate (subtract 1) (s-1))
  | q <- [n + x - s | n <- [1,3..]]
  ]

-- |Continued fraction representation of the upper incomplete gamma function.
-- Recommended for use when x >= s+1
upperGammaCF :: (Floating a, Ord a) => a -> a -> CF a
upperGammaCF s z = gcf 0
  [ (p,q)
  | p <- pow_x_s_div_exp_x s z
    : zipWith (*) (iterate (1+) 1) (iterate (subtract 1) (s-1))
  | q <- [n + z - s | n <- iterate (2+) 1]
  ]

-- |Natural logarithms of the convergents of the upper gamma function, 
-- evaluated carefully to avoid overflow and underflow.
-- Recommended for use when x >= s+1
lnUpperGammaConvergents :: (Eq a, Floating a) => a -> a -> [a]
lnUpperGammaConvergents s x = map (a -) (concat (eval theCF)) 
  where 
    eval = map (map evalSign) . modifiedLentzWith signLog addSignLog negateSignLog 1e-30
    
    a = s * log x - x
    theCF = gcf (x + 1 - s)
      [ (p,q)
      | p <- zipWith (*) (iterate (1+) 1) (iterate (subtract 1) (s-1))
      | q <- [n + x - s | n <- iterate (2+) 3]
      ]

---- various utility functions ----

evalSign :: Floating a => (a,a) -> a
evalSign (s,x) = log s + x

signLog :: Floating a => a -> (a,a)
signLog x = (signum x, log (abs x))

addSignLog :: (Num a, Num b) => (a,b) -> (a,b) -> (a,b)
addSignLog (xS,xL) (yS,yL) = (xS*yS, xL+yL)

negateSignLog :: (Num b) => (a,b) -> (a,b)
negateSignLog (s,l) = (s, negate l)

-- |Special case of Kummer's confluent hypergeometric function, used
-- in lower gamma functions.
-- 
-- m_1_sp1 s z = M(1;s+1;z)
-- 
m_1_sp1 :: (Eq a, Fractional a) => a -> a -> a
m_1_sp1 s z = converge . scanl (+) 0 . scanl (*) 1 $
    [z / x | x <- iterate (1+) (s+1)]

log_m_1_sp1 :: (Eq a, Floating a) => a -> a -> (a,a)
log_m_1_sp1 s z = converge (concat (log_m_1_sp1_convergents s z))

log_m_1_sp1_convergents :: (Eq a, Floating a) => a -> a -> [[(a,a)]]
log_m_1_sp1_convergents s z
  = modifiedLentzWith signLog addSignLog negateSignLog 1e-30
  $ sumPartialProducts (1:[z / x | x <- iterate (1+) (s+1)])

interleave :: [a] -> [a] -> [a]
interleave [] _ = []
interleave _ [] = []
interleave (x:xs) ys = x:interleave ys xs

-- A common subexpression appearing in both 'pCF' and 'qCF'.
pow_x_s_div_gamma_s_div_exp_x :: (Gamma a, Ord a) => a -> a -> a
pow_x_s_div_gamma_s_div_exp_x s x 
  | x > 0     = exp (log x * s - x - lnGamma s)
  | otherwise = x ** s / (exp x * gamma s)

-- The corresponding subexpression from 'lowerGammaCF' and 'upperGammaCF'
pow_x_s_div_exp_x :: (Floating a, Ord a) => a -> a -> a
pow_x_s_div_exp_x s x 
  | x > 0     = exp (log x * s - x)
  | otherwise = x ** s / exp x