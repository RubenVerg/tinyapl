module Main where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import TinyAPL.Parser
import qualified TinyAPL.Primitives as P
import TinyAPL.Interpreter

import Data.Complex
import System.Environment
import Control.Monad (void)
import System.IO
import Data.Functor (($>))
import Data.List (singleton)

main :: IO ()
main = do
  let a = vector $ map Number [1, 2, -1]
  let b = vector $ map Number [5, 2.1, 3 :+ (-0.5)]

  let i = arrayReshaped [3, 3] $ Number <$> [1, 0, 0
                                             , 0, 1, 0
                                             , 0, 0, 1 ]

  let inc = BindRight P.plus (scalar $ Number 1)
  
  putStrLn "a"; print a
  putStrLn "b"; print b
  putStrLn "i"; print i
  putStrLn "I"; print inc

  let scope = Scope [("a", a), ("b", b), ("i", i)] [("I", inc)] [] [] Nothing

  code <- unwords <$> getArgs
  if null code then repl scope
  else void $ runCode "<cli>" code scope

runCode :: String -> String -> Scope -> IO Scope
runCode file code scope = do
  result <- runResult $ run file code scope
  case result of
    Left err -> hPrint stderr err $> scope
    Right (res, scope) -> print res $> scope

repl :: Scope -> IO ()
repl scope = let
  go :: Scope -> IO Scope
  go scope = do
    putStr "> "
    hFlush stdout
    line <- getLine
    if line == "" then return scope
    else runCode "<repl>" line scope >>= go
  in do
    putStrLn "TinyAPL REPL, empty line to exit"
    putStrLn "Supported primitives:"
    putStrLn $ "  " ++ unwords (singleton . fst <$> P.arrays)
    putStrLn $ "  " ++ unwords (singleton . fst <$> P.functions)
    putStrLn $ "  " ++ unwords (singleton . fst <$> P.adverbs)
    putStrLn $ "  " ++ unwords (singleton . fst <$> P.conjunctions)
    putStrLn "Supported features:"
    putStrLn "* dfns {code}, d-monadic-ops _{code}, d-dyadic-ops _{code}_"
    putStrLn $ "  " ++ [G.alpha] ++ " left argument, " ++ [G.omega] ++ " right argument,"
    putStrLn $ "  " ++ [G.alpha, G.alpha] ++ " left array operand, " ++ [G.alphaBar, G.alphaBar] ++ " left function operand, " ++ [G.omega, G.omega] ++ " right array operand, " ++ [G.omegaBar, G.omegaBar] ++ " right function operand,"
    putStrLn $ "  " ++ [G.del] ++ " recurse function, " ++ ['_', G.del] ++ " recurse monadic op, " ++ ['_', G.del, '_'] ++ " recurse dyadic op"
    putStrLn $ "  " ++ [G.exit] ++ " early exit, " ++ [G.guard] ++ " guard"
    putStrLn $ "  " ++ [G.separator] ++ " multiple statements"
    putStrLn $ "* numbers: " ++ [G.decimal] ++ " decimal separator, " ++ [G.negative] ++ " negative sign, " ++ [G.exponent] ++ " exponent notation, " ++ [G.imaginary] ++ " complex separator"
    putStrLn $ "* character literals: " ++ [G.charDelimiter] ++ "abc" ++ [G.charDelimiter]
    putStrLn $ "* string literals: " ++ [G.stringDelimiter] ++ "abc" ++ [G.stringDelimiter] ++ " with escapes using " ++ [G.stringEscape]
    putStrLn $ "* names: abc array, Abc function, _Abc monadic op, _Abc_ dyadic op, assignment with " ++ [G.assign]
    putStrLn $ "* get " ++ [G.quad] ++ " read evaluated input, get " ++ [G.quadQuote] ++ " read string input, set " ++ [G.quad] ++ " print with newline, set " ++ [G.quadQuote] ++ " print without newline"
    void $ go scope 
