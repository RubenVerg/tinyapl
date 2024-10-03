module TinyAPL.FFI where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import TinyAPL.Util

import qualified Foreign.LibFFI as FFI
import System.Posix.DynamicLinker
import Foreign (FunPtr)
import Data.Functor
import Control.Monad

type DynamicLibrary = DL

loadLibrary :: String -> St DynamicLibrary
loadLibrary = liftToSt . flip dlopen [RTLD_NOW]

unloadLibrary :: DynamicLibrary -> St ()
unloadLibrary = liftToSt . dlclose

getFun :: DynamicLibrary -> String -> St (FunPtr a)
getFun = liftToSt .: dlsym

type Arg = Array -> St [FFI.Arg]
type RetType = FFI.RetType (St Array)

retVoid :: RetType
retVoid = FFI.retVoid $> pure (vector [])

argInt :: Arg
argInt arr = do
  let err = DomainError "Argument of type int must be a scalar integer"
  i <- asScalar err arr >>= asNumber err >>= asInt err
  pure $ [FFI.argCInt i]

retInt :: RetType
retInt = FFI.retCInt <&> (pure . scalar . Number . fromInteger . toInteger)

callFFI :: FunPtr a -> FFI.RetType (St r) -> [St [FFI.Arg]] -> St r
callFFI fun ret args = do
  args' <- concat <$> sequence args
  join $ liftToSt $ FFI.callFFI fun ret args'
