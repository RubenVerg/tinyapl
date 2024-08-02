module Main (main) where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.CoreQuads
import TinyAPL.Error
import TinyAPL.Glyphs (syntax, identifiers, arrays, functions, adverbs, conjunctions, quad)
import TinyAPL.Highlighter
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
foreign import javascript unsafe "return $1;" intToVal :: Int -> JSVal

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = return ()

{-# NOINLINE contexts #-}
contexts :: IORef [Context]
contexts = unsafePerformIO $ newIORef []

{-# NOINLINE lasts #-}
lasts :: IORef [Maybe Value]
lasts = unsafePerformIO $ newIORef []

noLast :: Error
noLast = DomainError "Last not found or has wrong type"

foreign import javascript safe "return await $1();" callInput :: JSVal -> IO JSString
foreign import javascript safe "await $1($2);" callOutput :: JSVal -> JSString -> IO ()

foreign export javascript "tinyapl_newContext" newContext :: JSVal -> JSVal -> JSVal -> IO Int

newContext :: JSVal -> JSVal -> JSVal -> IO Int
newContext input output error = do
  l <- length <$> readIORef contexts
  let readLast = (!! l) <$> (liftToSt $ readIORef lasts)
  modifyIORef contexts (++ [Context
    { contextScope = Scope [] [] [] [] Nothing
    , contextQuads = core <> quadsFromReprs [Nilad (Just $ do
      l <- readLast
      case l of
        Just (VArray arr) -> return arr
        _ -> throwError noLast
    ) Nothing (quad : "last")] [Function (Just $ \y -> do
      l <- readLast
      case l of
        Just (VFunction f) -> callMonad f y
        _ -> throwError noLast
    ) (Just $ \x y -> do
      l <- readLast
      case l of
        Just (VFunction f) -> callDyad f x y
        _ -> throwError noLast
    ) (quad : "Last")] [Adverb (Just $ \u -> do
      l <- readLast
      case l of
        Just (VAdverb adv) -> callOnArray adv u
        _ -> throwError noLast
    ) (Just $ \f -> do
      l <- readLast
      case l of
        Just (VAdverb adv) -> callOnFunction adv f
        _ -> throwError noLast
    ) (quad : "_Last")] [Conjunction (Just $ \u v -> do
      l <- readLast
      case l of
        Just (VConjunction conj) -> callOnArrayAndArray conj u v
        _ -> throwError noLast
    ) (Just $ \u g -> do
      l <- readLast
      case l of
        Just (VConjunction conj) -> callOnArrayAndFunction conj u g
        _ -> throwError noLast
    ) (Just $ \f v -> do
      l <- readLast
      case l of
        Just (VConjunction conj) -> callOnFunctionAndArray conj f v
        _ -> throwError noLast
    ) (Just $ \f g -> do
      l <- readLast
      case l of
        Just (VConjunction conj) -> callOnFunctionAndFunction conj f g
        _ -> throwError noLast
    ) (quad : "_Last_")]
    , contextIn = liftToSt $ fromJSString <$> callInput input
    , contextOut = \str -> liftToSt $ callOutput output $ toJSString str
    , contextErr = \str -> liftToSt $ callOutput error $ toJSString str }])
  modifyIORef lasts (++ [Nothing])
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
      modifyIORef lasts (setAt contextId $ Just res)
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

foreign export javascript "tinyapl_highlight" jsHighlight :: JSString -> JSVal

jsHighlight :: JSString -> JSVal
jsHighlight = toJSArray . map (intToVal . fromEnum). highlight . fromJSString

foreign export javascript "tinyapl_splitString" splitString :: JSString -> JSVal

splitString :: JSString -> JSVal
splitString = toJSArray . map toJSChar . fromJSString

foreign export javascript "tinyapl_hlOther" hlOther :: Int
foreign export javascript "tinyapl_hlSyntax" hlSyntax :: Int
foreign export javascript "tinyapl_hlNumber" hlNumber :: Int
foreign export javascript "tinyapl_hlString" hlString :: Int
foreign export javascript "tinyapl_hlStringEscape" hlStringEscape :: Int
foreign export javascript "tinyapl_hlArrayName" hlArrayName :: Int
foreign export javascript "tinyapl_hlPrimArray" hlPrimArray :: Int
foreign export javascript "tinyapl_hlFunctionName" hlFunctionName :: Int
foreign export javascript "tinyapl_hlPrimFunction" hlPrimFunction :: Int
foreign export javascript "tinyapl_hlAdverbName" hlAdverbName :: Int
foreign export javascript "tinyapl_hlPrimAdverb" hlPrimAdverb :: Int
foreign export javascript "tinyapl_hlConjunctionName" hlConjunctionName :: Int
foreign export javascript "tinyapl_hlPrimConjunction" hlPrimConjunction :: Int
foreign export javascript "tinyapl_hlComment" hlComment :: Int

hlOther = fromEnum COther
hlSyntax = fromEnum CSyntax
hlNumber = fromEnum CNumber
hlString = fromEnum CString
hlStringEscape = fromEnum CStringEscape
hlArrayName = fromEnum CArrayName
hlPrimArray = fromEnum CPrimArray
hlFunctionName = fromEnum CFunctionName
hlPrimFunction = fromEnum CPrimFunction
hlAdverbName = fromEnum CAdverbName
hlPrimAdverb = fromEnum CPrimAdverb
hlConjunctionName = fromEnum CConjunctionName
hlPrimConjunction = fromEnum CPrimConjunction
hlComment = fromEnum CComment
