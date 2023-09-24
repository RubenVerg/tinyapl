module Main where

import TinyAPL.Array
import Data.Complex

main :: IO ()
main = do
  let a = vector $ map Number [1, 2, -1]
  let b = vector $ map Number [5, 2.1, 3 :+ (-0.5)]
  
  putStr "a\t"; print a
  putStr "b\t"; print b
  putStr "a + b\t"; print $ a + b
  putStr "a - b\t"; print $ a - b
  putStr "a * b\t"; print $ a * b
  putStr "a / b\t"; print $ a / b
  putStr "sin a\t"; print $ sin a
