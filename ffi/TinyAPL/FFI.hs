{-# LANGUAGE CPP #-}

module TinyAPL.FFI where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import TinyAPL.Util

import qualified Foreign.LibFFI as FFI
import Foreign.Ptr
import Data.Functor
import Control.Monad
import Control.Monad.Catch
#if defined(mingw32_HOST_OS)
import qualified System.Win32.DLL as Win32
import qualified System.Win32.Types as Win32
#else
import System.Posix.DynamicLinker
#endif

libcPath :: FilePath
#if defined(mingw32_HOST_OS)
libcPath = "msvcrt.dll"
#elif defined(darwin_HOST_OS)
libcPath = "libc.dylib"
#else
libcPath = ""
#endif

libraryExtension :: String
#if defined(mingw32_HOST_OS)
libraryExtension = "dll"
#elif defined(darwin_HOST_OS)
libraryExtension = "dylib"
#else
libraryExtension = "so"
#endif

#if defined(MIN_VERSION_Win32)
type DynamicLibrary = Win32.HMODULE

loadLibrary :: String -> St DynamicLibrary
loadLibrary = liftToSt . Win32.loadLibrary

unloadLibrary :: DynamicLibrary -> St ()
unloadLibrary = liftToSt . Win32.freeLibrary

getFun :: DynamicLibrary -> String -> St (FunPtr a)
getFun lib name = do
  addr <- liftToSt $ Win32.getProcAddress lib name
  pure $ castPtrToFunPtr addr
#else
type DynamicLibrary = DL

loadLibrary :: String -> St DynamicLibrary
loadLibrary = liftToSt . flip dlopen [RTLD_NOW]

unloadLibrary :: DynamicLibrary -> St ()
unloadLibrary = liftToSt . dlclose

getFun :: DynamicLibrary -> String -> St (FunPtr a)
getFun = liftToSt .: dlsym
#endif

withLibrary :: String -> (DynamicLibrary -> St r) -> St r
withLibrary path f = bracket (loadLibrary path) unloadLibrary f

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
