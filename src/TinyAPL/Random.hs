module TinyAPL.Random
  ( random
  , randomR
  , setSeed )
where

import qualified System.Random as R
import System.IO.Unsafe
import Data.IORef
import Control.Monad.IO.Class

{-# NOINLINE theGenerator #-}
theGenerator :: IORef R.StdGen
theGenerator = unsafePerformIO $ newIORef $ R.mkStdGen 314159265

withTheGenerator :: (R.Random a, MonadIO m) => (R.StdGen -> (a, R.StdGen)) -> m a
withTheGenerator f = do
  gen <- liftIO $ readIORef theGenerator
  let (a, gen') = f gen
  liftIO $ writeIORef theGenerator gen'
  pure a

random :: (R.Random a, MonadIO m) => m a
random = withTheGenerator R.random

randomR :: (R.Random a, MonadIO m) => (a, a) -> m a
randomR = withTheGenerator . R.randomR

setSeed :: MonadIO m => Int -> m ()
setSeed s = liftIO $ writeIORef theGenerator $ R.mkStdGen s