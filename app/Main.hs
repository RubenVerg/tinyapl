{-# LANGUAGE CPP #-}

module Main where

import TinyAPL.CLI

#ifdef wasm32_HOST_ARCH
foreign export ccall "_start" main :: IO ()
#endif

main :: IO ()
main = cli
