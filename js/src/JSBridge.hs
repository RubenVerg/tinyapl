{-# LANGUAGE FlexibleInstances #-}

module JSBridge(IsJS(..), IsJSSt(..), JSArray(..), jsToString, jsNil, (++#), jsHead, jsTail, jsLength, jsUndefined, valToObject, objectToVal) where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import TinyAPL.Interpreter

import GHC.Wasm.Prim
import Numeric.Natural
import TinyAPL.Complex
import Data.List
import Data.Bifunctor

class IsJS a where
  fromJSVal :: JSVal -> a
  toJSVal :: a -> JSVal

class IsJSSt a where
  fromJSValSt :: JSVal -> St a
  toJSValSt :: a -> St JSVal

instance IsJS JSVal where
  fromJSVal = id
  toJSVal = id

instance IsJS () where
  fromJSVal = const ()
  toJSVal = const jsUndefined

foreign import javascript unsafe "return $1;" valToString :: JSVal -> JSString
foreign import javascript unsafe "return $1;" stringToVal :: JSString -> JSVal

instance IsJS JSString where
  fromJSVal = valToString
  toJSVal = stringToVal

instance IsJS Char where
  fromJSVal = head . fromJSString . fromJSVal
  toJSVal = toJSVal . toJSString . singleton

newtype JSArray = JSArray { unJSArray :: JSVal }

instance IsJS JSArray where
  fromJSVal = JSArray
  toJSVal = unJSArray

foreign import javascript unsafe "return [];" jsNil_ :: Bool -> JSArray
foreign import javascript unsafe "return [$1, ...$2];" jsCons :: JSVal -> JSArray -> JSArray
foreign import javascript unsafe "return $1[0];" jsHead :: JSArray -> JSVal
foreign import javascript unsafe "return $1.slice(1);" jsTail :: JSArray -> JSArray
foreign import javascript unsafe "return $1.length;" jsLength :: JSArray -> Int
foreign import javascript unsafe "return $1[$2];" jsAt :: JSArray -> Int -> JSVal
foreign import javascript unsafe "$1.push($2); return $1;" jsPush :: JSArray -> JSVal -> JSArray

foreign import javascript unsafe "return $1.toString();" jsToString_ :: JSVal -> JSString

jsToString :: IsJS a => a -> String
jsToString = fromJSString . jsToString_ . toJSVal

instance Show JSVal where
  show = jsToString

bamboozle :: [a] -> Bool
bamboozle [] = False
bamboozle _ = True
{-# NOINLINE bamboozle #-}

jsNil :: JSArray
jsNil = jsNil_ False

infixr 5 ++#
(++#) :: IsJS a => a -> JSArray -> JSArray
(++#) x = jsCons $ toJSVal x

listToArray :: IsJS a => [a] -> JSArray
listToArray xs = foldl' (\arr x -> jsPush arr $ toJSVal x) (jsNil_ $ bamboozle xs) xs

arrayToList :: IsJS a => JSArray -> [a]
arrayToList arr = fromJSVal . jsAt arr <$> [0..jsLength arr-1]

instance IsJS a => IsJS [a] where
  fromJSVal = arrayToList . fromJSVal
  toJSVal = toJSVal . listToArray

foreign import javascript unsafe "return undefined;" jsUndefined :: JSVal
foreign import javascript unsafe "return $1 === undefined;" jsIsUndefined :: JSVal -> Bool

instance IsJS a => IsJS (Maybe a) where
  fromJSVal x = if jsIsUndefined x then Nothing else Just $ fromJSVal x
  toJSVal (Just x) = toJSVal x
  toJSVal Nothing = jsUndefined

foreign import javascript unsafe "return $1;" valToDouble :: JSVal -> Double
foreign import javascript unsafe "return $1;" doubleToVal :: Double -> JSVal

instance IsJS Double where
  fromJSVal = valToDouble
  toJSVal = doubleToVal

foreign import javascript unsafe "return $1;" valToInt :: JSVal -> Int
foreign import javascript unsafe "return $1;" intToVal :: Int -> JSVal

instance IsJS Int where
  fromJSVal = valToInt
  toJSVal = intToVal

instance IsJS Natural where
  fromJSVal = toEnum . fromJSVal
  toJSVal = toJSVal . fromEnum

foreign import javascript unsafe "return $1;" valToBool :: JSVal -> Bool
foreign import javascript unsafe "return $1;" boolToVal :: Bool -> JSVal

instance IsJS Bool where
  fromJSVal = valToBool
  toJSVal = boolToVal

instance IsJS (Complex Double) where
  fromJSVal xs = let [r, i] = arrayToList $ fromJSVal xs in r :+ i
  toJSVal (r :+ i) = toJSVal $ listToArray [r, i]

foreign import javascript unsafe "return [$1, $2];" jsPair :: JSVal -> JSVal -> JSVal
foreign import javascript unsafe "return $1[0];" jsFst :: JSVal -> JSVal
foreign import javascript unsafe "return $1[1];" jsSnd :: JSVal -> JSVal

instance (IsJS a, IsJS b) => IsJS (a, b) where
  fromJSVal xy = (fromJSVal $ jsFst xy, fromJSVal $ jsSnd xy)
  toJSVal (x, y) = jsPair (toJSVal x) (toJSVal y)

foreign import javascript unsafe "return Object.entries($1);" jsEntries :: JSVal -> JSArray
foreign import javascript unsafe "return Object.fromEntries($1);" jsFromEntries :: JSArray -> JSVal

valToObject :: JSVal -> [(String, JSVal)]
valToObject = map (first fromJSString) . fromJSVal . toJSVal . jsEntries

objectToVal :: [(String, JSVal)] -> JSVal
objectToVal = jsFromEntries . fromJSVal . toJSVal . map (first toJSString)

foreign import javascript unsafe "return typeof $1;" jsTypeOf :: JSVal -> JSString
foreign import javascript unsafe "return Array.isArray($1);" jsIsArray :: JSVal -> Bool

instance IsJS ScalarValue where
  fromJSVal v = case fromJSString $ jsTypeOf v of
    "number" -> Number $ fromJSVal v :+ 0
    "string" -> Character $ head $ fromJSVal v
    "object" ->
      if jsIsArray v then Number $ fromJSVal v
      else Box $ fromJSVal v
    _ -> error "fromJSVal ScalarValue: wrong type"
  toJSVal (Number x) = toJSVal x
  toJSVal (Character x) = toJSVal x
  toJSVal (Box xs) = toJSVal xs

foreign import javascript unsafe "return $1[$2];" jsLookup :: JSVal -> JSString -> JSVal

instance IsJS Array where
  fromJSVal v
    | fromJSVal (jsLookup v $ toJSString "type") == "array" = let
      shape = arrayToList $ fromJSVal $ jsLookup v $ toJSString "shape"
      contents = arrayToList $ fromJSVal $ jsLookup v $ toJSString "contents"
      in Array shape contents
    | otherwise = error "fromJSVal Array: not an array"
  toJSVal (Array shape contents) = objectToVal [("type", toJSVal $ toJSString "array"), ("shape", toJSVal shape), ("contents", toJSVal contents)]

instance IsJS Error where
  fromJSVal v = let
    typ = fromJSVal $ jsLookup v $ toJSString "code"
    message = fromJSString $ fromJSVal $ jsLookup v $ toJSString "message"
    in fromErrorCode typ message
  toJSVal err = objectToVal [("code", toJSVal $ errorCode err), ("message", toJSVal $ toJSString $ errorMessage err)]

foreign import javascript unsafe "return $1 in $2;" jsIn :: JSString -> JSVal -> Bool

instance IsJS a => IsJS (Either Error a) where
  fromJSVal v = if jsIn (toJSString "code") v then Left $ fromJSVal v else Right $ fromJSVal v
  toJSVal (Left err) = toJSVal err
  toJSVal (Right x) = toJSVal x

instance IsJSSt a => IsJSSt (Either Error a) where
  fromJSValSt v = if jsIn (toJSString "code") v then pure $ Left $ fromJSVal v else Right <$> fromJSValSt v
  toJSValSt (Left err) = pure $ toJSVal err
  toJSValSt (Right x) = toJSValSt x

foreign import javascript safe "return await $1($2);" jsCall1 :: JSVal -> JSVal -> IO JSVal
foreign import javascript safe "return await $1($2, $3);" jsCall2 :: JSVal -> JSVal -> JSVal -> IO JSVal

foreign import javascript unsafe "wrapper" jsWrap1 :: (JSVal -> IO JSVal) -> IO JSVal
foreign import javascript unsafe "wrapper" jsWrap2 :: (JSVal -> JSVal -> IO JSVal) -> IO JSVal

instance IsJSSt Function where
  fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "function" = do
      let repr = fromJSString $ fromJSVal $ jsLookup v $ toJSString "repr"
      let monad = jsLookup v $ toJSString "monad"
      let dyad = jsLookup v $ toJSString "dyad"
      sc <- createRef $ Scope [] [] [] [] Nothing
      ctx <- getContext
      pure $ Function {
        functionRepr = repr,
        functionContext = Just $ ctx{ contextScope = sc },
        functionMonad = if jsIsUndefined monad then Nothing else Just $ (\x -> liftToSt $ fromJSVal <$> jsCall1 monad (toJSVal x)),
        functionDyad = if jsIsUndefined dyad then Nothing else Just $ (\x y -> liftToSt $ fromJSVal <$> jsCall2 dyad (toJSVal x) (toJSVal y)) }
    | otherwise = throwError $ DomainError "fromJSValSt Function: not a function"
  toJSValSt f = do
    ctx <- getContext
    monad <- liftToSt $ jsWrap1 $ \x -> toJSVal . second fst <$> (runResult $ runSt (callMonad f (fromJSVal x)) ctx)
    dyad <- liftToSt $ jsWrap2 $ \x y -> toJSVal . second fst <$> (runResult $ runSt (callDyad f (fromJSVal x) (fromJSVal y)) ctx)
    pure $ objectToVal [("type", toJSVal $ toJSString "function"), ("repr", toJSVal $ toJSString $ functionRepr f), ("monad", monad), ("dyad", dyad)]

instance IsJSSt Value where
  fromJSValSt v
    | fromJSVal (jsLookup v $ toJSString "type") == "array" = pure $ VArray $ fromJSVal v
    | fromJSVal (jsLookup v $ toJSString "type") == "function" = VFunction <$> fromJSValSt v
    | otherwise = throwError $ DomainError "fromJSValSt Value: unknown type"
  toJSValSt (VArray arr) = pure $ toJSVal arr
  toJSValSt (VFunction f) = toJSValSt f
  toJSValSt _ = throwError $ DomainError "toJSValSt Value: unsupported type"
