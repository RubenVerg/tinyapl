{-# LANGUAGE FlexibleInstances #-}

module JSBridge(IsJS(..), JSArray(..), jsNil, (++#), jsHead, jsTail, jsLength, jsUndefined, valToObject, objectToVal) where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error

import GHC.Wasm.Prim
import Numeric.Natural
import Data.Complex
import Data.List
import Data.Bifunctor

class IsJS a where
  fromJSVal :: JSVal -> a
  toJSVal :: a -> JSVal

instance IsJS JSVal where
  fromJSVal = id
  toJSVal = id

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

foreign import javascript unsafe "return [];" jsNil :: JSArray
foreign import javascript unsafe "return [$1, ...$2];" jsCons :: JSVal -> JSArray -> JSArray
foreign import javascript unsafe "return $1[0];" jsHead :: JSArray -> JSVal
foreign import javascript unsafe "return $1.slice(1);" jsTail :: JSArray -> JSArray
foreign import javascript unsafe "return $1.length;" jsLength :: JSArray -> Int

infixr 5 ++#
(++#) :: IsJS a => a -> JSArray -> JSArray
(++#) x = jsCons $ toJSVal x

listToArray :: IsJS a => [a] -> JSArray
listToArray = foldr (++#) jsNil

arrayToList :: IsJS a => JSArray -> [a]
arrayToList arr = if jsLength arr == 0 then [] else fromJSVal (jsHead arr) : arrayToList (jsTail arr)

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
  fromJSVal v = let
    shape = arrayToList $ fromJSVal $ jsLookup v $ toJSString "shape"
    contents = arrayToList $ fromJSVal $ jsLookup v $ toJSString "contents"
    in Array shape contents
  toJSVal (Array shape contents) = objectToVal [("shape", toJSVal shape), ("contents", toJSVal contents)]

instance IsJS Error where
  fromJSVal v = let
    typ = fromJSVal $ jsLookup v $ toJSString "code"
    message = fromJSString $ fromJSVal $ jsLookup v $ toJSString "message"
    in fromErrorCode typ message
  toJSVal err = objectToVal [("code", toJSVal $ errorCode err), ("message", toJSVal $ toJSString $ errorMessage err)]

foreign import javascript unsafe "return $1 in $2;" jsIn :: JSString -> JSVal -> Bool

instance IsJS (Either Error Array) where
  fromJSVal v = if jsIn (toJSString "code") v then Left $ fromJSVal v else Right $ fromJSVal v
  toJSVal (Left err) = toJSVal err
  toJSVal (Right x) = toJSVal x