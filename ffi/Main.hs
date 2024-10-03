module Main (main) where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import TinyAPL.FFI
import TinyAPL.Util

import Data.IORef
import Control.Monad
import Control.Monad.Catch

testPath :: String
testPath = "./ffi/dist/test.so"

main' :: St ()
main' = bracket (loadLibrary testPath) unloadLibrary $ \lib -> do
  printHello <- getFun lib "printHello"
  add <- getFun lib "add"
  void $ callFFI printHello retVoid []
  liftToSt $ putStr "result of add 2 and 3: "
  liftToSt . print =<< callFFI add retInt [argInt $ scalar $ Number 2, argInt $ scalar $ Number 3]

main :: IO ()
main = do
  counter <- newIORef 0
  scope <- newIORef $ Scope [] [] [] [] Nothing
  fst . fromRight' <$> (runResult $ runSt main' (Context scope mempty undefined undefined undefined counter))