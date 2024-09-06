{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveTraversable, FlexibleInstances, TypeFamilies, TypeApplications, ScopedTypeVariables, MultiParamTypeClasses #-}

module TinyAPL.Complex
  ( Complex(..)
  , realPart
  , imagPart
  , conjugate
  , mkPolar
  , cis
  , polar
  , magnitude
  , phase
  , toStd
  , fromStd ) where

import GHC.Generics
import Data.Data
import Numeric
import Foreign.Storable
import Foreign.Ptr
import Control.Monad.Zip
import Control.Monad.Fix
import qualified Data.Complex as Cx
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VGM
import qualified Data.Vector.Unboxed as VU
import Data.Coerce
import Control.DeepSeq

infix 6 :+

data Complex a = !a :+ !a
  deriving (Eq, Show, Read, Data, Generic, Generic1, Functor, Foldable, Traversable)

realPart :: Complex a -> a
realPart (x :+ _) = x

imagPart :: Complex a -> a
imagPart (_ :+ y) = y

{-# SPECIALIZE conjugate :: Complex Double -> Complex Double #-}
conjugate :: Num a => Complex a -> Complex a
conjugate (x :+ y) = x :+ negate y

{-# SPECIALIZE mkPolar :: Double -> Double -> Complex Double #-}
mkPolar :: Floating a => a -> a -> Complex a
mkPolar r theta = r * cos theta :+ r * sin theta

{-# SPECIALIZE cis :: Double -> Complex Double #-}
cis :: Floating a => a -> Complex a
cis theta = cos theta :+ sin theta

{-# SPECIALIZE polar :: Complex Double -> (Double, Double) #-}
polar :: RealFloat a => Complex a -> (a, a)
polar z = (magnitude z, phase z)

{-# SPECIALISE magnitude :: Complex Double -> Double #-}
magnitude :: (RealFloat a) => Complex a -> a
magnitude (x:+y) =  scaleFloat k
                     (sqrt (sqr (scaleFloat mk x) + sqr (scaleFloat mk y)))
                    where k  = max (exponent x) (exponent y)
                          mk = - k
                          sqr z = z * z

{-# SPECIALIZE phase :: Complex Double -> Double #-}
phase :: RealFloat a => Complex a -> a
phase (x :+ y) = let inf = 1 / 0 in
  if (x, y) == (0, 0) then 0
  else if (x, y) == (inf, inf) then atan2 1 1
  else if (x, y) == (inf, -inf) then atan2 1 (-1)
  else if (x, y) == (-inf, inf) then atan2 (-1) 1
  else if (x, y) == (-inf, -inf) then atan2 (-1) (-1)
  else atan2 y x

instance RealFloat a => Num (Complex a) where
  {-# SPECIALIZE instance Num (Complex Double) #-}
  (x1 :+ y1) + (x2 :+ y2) = (x1 + x2) :+ (y1 + y2)
  (x1 :+ y1) - (x2 :+ y2) = (x1 - x2) :+ (y1 - y2)
  z1@(x1 :+ y1) * z2@(x2 :+ y2) = let
    re1 = x1 * x2
    re2 = y1 * y2
    im1 = x1 * y2
    im2 = x2 * y1
    usePolar = (isInfinite re1 && isInfinite re2 && signum re1 == signum re2)
            || (isInfinite im1 && isInfinite im2 && signum im1 /= signum im2)
    (r1, t1) = polar z1
    (r2, t2) = polar z2
    in if usePolar then (r1 * r2) `mkPolar` (t1 + t2) else (re1 - re2) :+ (im1 + im2)
  negate (x :+ y) = negate x :+ negate y
  abs z = magnitude z :+ 0
  signum (0 :+ 0) = 0
  signum z@(x :+ y) = x / r :+ y / r where r = magnitude z
  fromInteger n = fromInteger n :+ 0

instance RealFloat a => Fractional (Complex a) where
  {-# SPECIALIZE instance Fractional (Complex Double) #-}
  z1@(x1 :+ y1) / z2@(x2 :+ y2) = let
    x' = scaleFloat k x2
    y' = scaleFloat k y2
    k = negate $ exponent x2 `max` exponent y2
    d = x2 * x' + y2 * y'
    re1 = x1 * x'
    re2 = y1 * y'
    im1 = y1 * x'
    im2 = x1 * y'
    usePolar = (isInfinite re1 && isInfinite re2 && signum re1 /= signum re2)
            || (isInfinite im1 && isInfinite im2 && signum im1 == signum im2)
    (r1, t1) = polar z1
    (r2, t2) = polar z2
    in if usePolar then (r1 / r2) `mkPolar` (t1 - t2) else (re1 + re2) / d :+ (im1 - im2) / d
  fromRational a = fromRational a :+ 0

instance RealFloat a => Floating (Complex a) where
  {-# SPECIALIZE instance Floating (Complex Double) #-}
  pi = pi :+ 0
  exp (x :+ y) = let expx = exp x in expx * cos y :+ expx * sin y
  log z = log (magnitude z) :+ phase z
  x ** y = case (x, y) of
    (_, 0) -> 1
    (0, (e :+ _)) -> case compare e 0 of
      GT -> 0
      LT -> inf :+ 0
      EQ -> nan :+ nan
    ((re :+ im), (e :+ _))
      | isInfinite re || isInfinite im -> case compare e 0 of
        GT -> inf :+ 0
        LT -> 0 :+ 0
        EQ -> nan :+ nan
      | otherwise -> exp $ log x * y
    where
      inf = 1 / 0
      nan = 0 / 0
  sqrt 0 = 0
  sqrt z@(x:+y) = u :+ (if y < 0 then negate v else v) where
    (u,v) = if x < 0 then (v',u') else (u',v')
    v' = abs y / (u' * 2)
    u' = sqrt $ (magnitude z + abs x) / 2
  sin (x :+ y) = sin x * cosh y :+ cos x * sinh y
  cos (x :+ y) = cos x * cosh y :+ negate (sin x * sinh y)
  tan z = sin z / cos z
  sinh (x :+ y) = cos y * sinh x :+ sin y * cosh x
  cosh (x :+ y) = cos y * cosh x :+ sin y * sinh x
  tanh z = sinh z / cosh z
  asin z@(x :+ y) = y' :+ negate x' where
    (x' :+ y') = log $ (negate y :+ x) + sqrt (1 - z * z)
  acos z = y'' :+ negate x'' where
    (x'' :+ y'') = log $ z + (negate y' :+ x')
    (x' :+ y') = sqrt $ 1 - z * z
  atan z@(x :+ y) = y' :+ (-x') where
    (x' :+ y') = log $ ((1 - y) :+ x) / sqrt (1 + z * z)
  asinh z = log $ z + sqrt (1 + z * z)
  acosh z = log $ z + sqrt (z + 1) * sqrt (z - 1)
  atanh z = 0.5 * log ((1 + z) / (1 - z))
  log1p z@(x :+ y)
    | abs x < 0.5 && abs y < 0.6
    , u <- 2 * x + x * x + y * y = log1p (u / (1 + sqrt (u + 1))) :+ atan2 (1 + x) y
    | otherwise = log $ 1 + z
  {-# INLINE log1p #-}
  expm1 z@(x :+ y)
    | x * x + y * y < 1
    , u <- expm1 x
    , v <- sin $ y / 2
    , w <- negate $ 2 * v * v = (u * w + u + w) :+ (u + 1) * sin y
    | otherwise = exp z - 1
  {-# INLINE expm1 #-}

instance Storable a => Storable (Complex a) where
  sizeOf (x :+ _) = 2 * sizeOf x
  alignment (x :+ _) = alignment x
  peek p = do
    q <- return $ castPtr p
    x <- peek q
    y <- peekElemOff q 1
    return $ x :+ y
  poke p (x :+ y) = do
    q <- return $ castPtr p
    poke q x
    pokeElemOff q 1 y

instance Applicative Complex where
  pure a = a :+ a
  f :+ g <*> a :+ b = f a :+ g b
  liftA2 f (x :+ y) (a :+ b) = f x a :+ f y b

instance Monad Complex where
  a :+ b >>= f = realPart (f a) :+ imagPart (f b)

instance MonadZip Complex where
  mzipWith = liftA2

instance MonadFix Complex where
  mfix f = (let a :+ _ = f a in a) :+ (let _ :+ b = f b in b)

{-# RULES

"realToFrac/a->Complex Double"
  realToFrac = \x -> realToFrac x :+ (0 :: Double)

"realToFrac/a->Complex Float"
  realToFrac = \x -> realToFrac x :+ (0 :: Float)

#-}

toStd :: Complex a -> Cx.Complex a
toStd (x :+ y) = x Cx.:+ y

fromStd :: Cx.Complex a -> Complex a
fromStd (x Cx.:+ y) = x :+ y

newtype instance VU.MVector s (Complex a) = MV_Complex (VU.MVector s (a, a))
newtype instance VU.Vector (Complex a) = V_Complex (VU.Vector (a, a))

instance (VU.Unbox a) => VGM.MVector VU.MVector (Complex a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeMove #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength      = coerce $ VGM.basicLength      @VU.MVector @(a,a)
  basicUnsafeSlice = coerce $ VGM.basicUnsafeSlice @VU.MVector @(a,a)
  basicOverlaps    = coerce $ VGM.basicOverlaps    @VU.MVector @(a,a)
  basicUnsafeNew   = coerce $ VGM.basicUnsafeNew   @VU.MVector @(a,a)
  basicInitialize  = coerce $ VGM.basicInitialize  @VU.MVector @(a,a)
  basicUnsafeCopy  = coerce $ VGM.basicUnsafeCopy  @VU.MVector @(a,a)
  basicUnsafeMove  = coerce $ VGM.basicUnsafeMove  @VU.MVector @(a,a)
  basicUnsafeGrow  = coerce $ VGM.basicUnsafeGrow  @VU.MVector @(a,a)
  basicClear       = coerce $ VGM.basicClear       @VU.MVector @(a,a)
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicSet #-}
  basicUnsafeReplicate n (x :+ y) = MV_Complex <$> VGM.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Complex v) i = uncurry (:+) <$> VGM.basicUnsafeRead v i
  basicUnsafeWrite (MV_Complex v) i (x :+ y) = VGM.basicUnsafeWrite v i (x,y)
  basicSet (MV_Complex v) (x :+ y) = VGM.basicSet v (x,y)

instance (VU.Unbox a) => VG.Vector VU.Vector (Complex a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeFreeze = coerce $ VG.basicUnsafeFreeze @VU.Vector @(a,a)
  basicUnsafeThaw   = coerce $ VG.basicUnsafeThaw   @VU.Vector @(a,a)
  basicLength       = coerce $ VG.basicLength       @VU.Vector @(a,a)
  basicUnsafeSlice  = coerce $ VG.basicUnsafeSlice  @VU.Vector @(a,a)
  basicUnsafeCopy   = coerce $ VG.basicUnsafeCopy   @VU.Vector @(a,a)
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeIndexM (V_Complex v) i = uncurry (:+) <$> VG.basicUnsafeIndexM v i
  elemseq _ (x :+ y) z = VG.elemseq (undefined :: VU.Vector a) x
                       $ VG.elemseq (undefined :: VU.Vector a) y z

instance (VU.Unbox a) => VU.Unbox (Complex a)

instance NFData a => NFData (Complex a) where
  rnf (x :+ y) = rnf x `seq` rnf y `seq` ()
