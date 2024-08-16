module Main (main) where

import JSBridge

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
import Data.Function
import Data.Char

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
foreign import javascript safe "return await $1();" jsGetNilad :: JSVal -> IO JSVal
foreign import javascript safe "await $1($2);" jsSetNilad :: JSVal -> JSVal -> IO JSVal
foreign import javascript safe "return await $1($2);" jsCallMonad :: JSVal -> JSVal -> IO JSVal
foreign import javascript safe "return await $1($2, $3)" jsCallDyad :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign export javascript "tinyapl_newContext" newContext :: JSVal -> JSVal -> JSVal ->  JSVal -> IO Int

lastQuads :: Int -> Quads
lastQuads l = let readLast = (!! l) <$> (liftToSt $ readIORef lasts) in
  quadsFromReprs [Nilad (Just $ do
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
  ) (quad : "Last") Nothing] [Adverb (Just $ \u -> do
    l <- readLast
    case l of
      Just (VAdverb adv) -> callOnArray adv u
      _ -> throwError noLast
  ) (Just $ \f -> do
    l <- readLast
    case l of
      Just (VAdverb adv) -> callOnFunction adv f
      _ -> throwError noLast
  ) (quad : "_Last") Nothing] [Conjunction (Just $ \u v -> do
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
  ) (quad : "_Last_") Nothing]

newContext :: JSVal -> JSVal -> JSVal -> JSVal -> IO Int
newContext input output error quads = do
  l <- length <$> readIORef contexts
  let qs = valToObject quads
  let nilads = filter (isLower . head . fst) qs
  let functions = filter (isUpper . head . fst) qs
  emptyScope <- newIORef $ Scope [] [] [] [] Nothing
  modifyIORef contexts (++ [Context
    { contextScope = emptyScope
    , contextQuads = core <> lastQuads l <>
      quadsFromReprs
        ((\(n, f) -> Nilad (Just $ do
          r <- liftToSt $ fromJSVal <$> jsGetNilad f
          case r of
            Left err -> throwError err
            Right res -> pure res) (Just $ \y -> do
          r <- liftToSt $ fmap fromJSVal $ jsSetNilad f $ toJSVal y
          case r of
            Just err -> throwError err
            Nothing -> pure ()) (quad : n)) <$> nilads)
        ((\(n, f) -> Function (Just $ \y -> do
          r <- liftToSt $ fmap fromJSVal $ jsCallMonad f $ toJSVal y
          case r of
            Left err -> throwError err
            Right res -> pure res) (Just $ \x y -> do
          r <- liftToSt $ fmap fromJSVal $ (jsCallDyad f `on` toJSVal) x y
          case r of
            Left err -> throwError err
            Right res -> pure res) (quad : n) Nothing) <$> functions)
        [] []
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

foreign export javascript "tinyapl_runCode" runCodeJS :: Int -> JSString -> IO JSVal

runCodeJS :: Int -> JSString -> IO JSVal
runCodeJS contextId code = do
  (r, s) <- runCode contextId $ fromJSString code
  return $ toJSVal (r, s)
  
foreign export javascript "tinyapl_glyphsSyntax" glyphsSyntaxJS :: JSArray
foreign export javascript "tinyapl_glyphsIdentifiers" glyphsIdentifiersJS :: JSArray
foreign export javascript "tinyapl_glyphsArrays" glyphsArraysJS :: JSArray
foreign export javascript "tinyapl_glyphsFunctions" glyphsFunctionsJS :: JSArray
foreign export javascript "tinyapl_glyphsAdverbs" glyphsAdverbsJS :: JSArray
foreign export javascript "tinyapl_glyphsConjunctions" glyphsConjunctionsJS :: JSArray

glyphsSyntaxJS = fromJSVal . toJSVal $ toJSChar <$> syntax
glyphsIdentifiersJS = fromJSVal . toJSVal $ toJSChar <$> identifiers
glyphsArraysJS = fromJSVal . toJSVal $ toJSChar <$> arrays
glyphsFunctionsJS = fromJSVal . toJSVal $ toJSChar <$> functions
glyphsAdverbsJS = fromJSVal . toJSVal $ toJSChar <$> adverbs
glyphsConjunctionsJS = fromJSVal . toJSVal $ toJSChar <$> conjunctions

foreign export javascript "tinyapl_highlight" jsHighlight :: JSString -> JSArray

jsHighlight :: JSString -> JSArray
jsHighlight = fromJSVal . toJSVal . map (intToVal . fromEnum). highlight . fromJSString

foreign export javascript "tinyapl_splitString" splitString :: JSString -> JSArray

splitString :: JSString -> JSArray
splitString = fromJSVal . toJSVal . fromJSString

foreign export javascript "tinyapl_joinString" joinString :: JSArray -> JSString

joinString :: JSArray -> JSString
joinString = toJSString . fromJSVal . toJSVal

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

foreign export javascript "tinyapl_errUser" errUser :: Int
foreign export javascript "tinyapl_errDomain" errDomain :: Int
foreign export javascript "tinyapl_errLength" errLength :: Int
foreign export javascript "tinyapl_errRank" errRank :: Int
foreign export javascript "tinyapl_errNYI" errNYI :: Int
foreign export javascript "tinyapl_errSyntax" errSyntax :: Int
foreign export javascript "tinyapl_errAssertion" errAssertion :: Int

errUser = errorCode $ UserError ""
errDomain = errorCode $ DomainError ""
errLength = errorCode $ LengthError ""
errRank = errorCode $ RankError ""
errNYI = errorCode $ NYIError ""
errSyntax = errorCode $ SyntaxError ""
errAssertion = errorCode $ AssertionError ""
