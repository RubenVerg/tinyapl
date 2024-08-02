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

{-# NOINLINE contexts #-}
contexts :: IORef [Context]
contexts = unsafePerformIO $ newIORef []

foreign import javascript unsafe "prompt($1)" jsPrompt :: JSString -> IO JSString
foreign import javascript unsafe "alert($1)" jsAlert :: JSString -> IO ()
  
emptyContext :: Context
emptyContext = Context
  { contextScope = Scope [] [] [] [] Nothing
  , contextQuads = core
  , contextIn = fromJSString <$> (liftToSt $ jsPrompt $ toJSString "")
  , contextOut = liftToSt . jsAlert . toJSString
  , contextErr = liftToSt . jsAlert . toJSString }


foreign export javascript "tinyapl_newContext" newContext :: IO Int

newContext :: IO Int
newContext = do
  l <- length <$> readIORef contexts
  modifyIORef contexts (++ [emptyContext])
  return l

runCode :: Int -> String -> IO (String, Bool)
runCode contextId code = do
  context <- (!! contextId) <$> readIORef contexts
  let file = "<tinyapl js>"
  result <- runResult $ run file code context
  case result of
    Left err -> return (show err, False)
    Right (res, context') -> do
      modifyIORef contexts (setAt contextId context')
      return (show res, True)

foreign import javascript unsafe "return [$1, $2];" jsResultPair :: JSString -> Bool -> JSVal

foreign export javascript "tinyapl_runCode" runCodeJS :: Int -> JSString -> IO JSVal

runCodeJS :: Int -> JSString -> IO JSVal
runCodeJS contextId code = do
  (r, s) <- runCode contextId $ fromJSString code
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

