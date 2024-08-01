module Main (main) where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.CoreQuads
import TinyAPL.Error
import TinyAPL.Glyphs (syntax, identifiers, arrays, functions, adverbs, conjunctions)
import TinyAPL.Interpreter
import TinyAPL.Util

import Data.IORef
import GHC.Wasm.Prim
import System.IO.Unsafe
import Data.List

foreign import javascript unsafe "return [];" jsNil :: JSVal
foreign import javascript unsafe "const a = $2.slice(); a.unshift($1); return a;" jsCons :: JSVal -> JSVal -> JSVal

toJSArray :: [JSVal] -> JSVal
toJSArray = foldr jsCons jsNil

toJSChar :: Char -> JSVal
toJSChar = strToVal . toJSString . singleton

foreign import javascript unsafe "return $1;" strToVal :: JSString -> JSVal

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = return ()

scopes :: IORef [Scope]
scopes = unsafePerformIO $ newIORef []

foreign export javascript "tinyapl_newScope" newScope :: IO Int

newScope :: IO Int
newScope = do
  l <- length <$> readIORef scopes
  modifyIORef scopes (++ [Scope [] [] [] [] Nothing core])
  return l

runCode :: Int -> String -> IO (String, Bool)
runCode scopeId code = do
  scope <- (!! scopeId) <$> readIORef scopes
  let file = "<tinyapl js>"
  result <- runResult $ run file code scope
  case result of
    Left err -> return (show err, False)
    Right (res, scope') -> do
      modifyIORef scopes (setAt scopeId scope')
      return (show res, True)

foreign import javascript unsafe "return [$1, $2];" jsResultPair :: JSString -> Bool -> JSVal

foreign export javascript "tinyapl_runCode" runCodeJS :: Int -> JSString -> IO JSVal

runCodeJS :: Int -> JSString -> IO JSVal
runCodeJS scopeId code = do
  (r, s) <- runCode scopeId $ fromJSString code
  return $ jsResultPair (toJSString r) s
  
foreign export javascript "tinyapl_glyphsSyntax" glyphsSyntaxJS :: JSVal
foreign export javascript "tinyapl_glyphsIdentifiers" glyphsIdentifiersJS :: JSVal
foreign export javascript "tinyapl_glyphsArrays" glyphsArraysJS :: JSVal
foreign export javascript "tinyapl_glyphsFunctions" glyphsFunctionsJS :: JSVal
foreign export javascript "tinyapl_glyphsAdverbs" glyphsAdverbsJS :: JSVal
foreign export javascript "tinyapl_glyphsConjunctions" glyphsConjunctionsJS :: JSVal

glyphsSyntaxJS = toJSArray $ toJSChar <$> syntax
glyphsIdentifiersJS = toJSArray $ toJSChar <$> identifiers
glyphsArraysJS = toJSArray $ toJSChar <$> arrays
glyphsFunctionsJS = toJSArray $ toJSChar <$> functions
glyphsAdverbsJS = toJSArray $ toJSChar <$> adverbs
glyphsConjunctionsJS = toJSArray $ toJSChar <$> conjunctions

