module Main (main) where

import GHC.Wasm.Prim

foreign import javascript unsafe "console.log($1)" js_consoleLog :: JSString -> IO ()

foreign export javascript "hs_start" main :: IO ()

main :: IO ()
main = do
  js_consoleLog $ toJSString "Hello, JS!"