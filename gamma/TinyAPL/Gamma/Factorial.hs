{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-all #-}

module TinyAPL.Gamma.Factorial
  ( Factorial (..) ) where

import TinyAPL.Complex (Complex (..))
import qualified Data.Vector.Unboxed as V
import GHC.Float (double2Float)

-- |Factorial function
class Num a => Factorial a where
  factorial :: Integral b => b -> a
  factorial = fromInteger . factorial

instance Factorial Integer where
  factorial n
    | n < 0     = error "factorial: n < 0"
    | otherwise = product [1..toInteger n]

instance Factorial Float where
  factorial = double2Float . factorial
instance Factorial (Complex Float) where
  factorial = (:+ 0) . factorial
instance Factorial Double where
  factorial n
    | n < 0         = 0/0
    | n < nFacs     = facs V.! fromIntegral n
    | otherwise     = infinity
    where
      nFacs :: Num a => a
      nFacs       = 171 -- any more is pointless, everything beyond here is "Infinity"
      facs        = V.scanl (*) 1 (V.enumFromN 1 nFacs)
      infinity    = facs V.! nFacs
instance Factorial (Complex Double) where
  factorial = (:+ 0) . factorial