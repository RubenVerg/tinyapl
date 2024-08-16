module Main where

import TinyAPL.ArrayFunctionOperator
import TinyAPL.CoreQuads
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import qualified TinyAPL.Primitives as P
import TinyAPL.Interpreter

import System.Environment
import Control.Monad (void)
import System.IO
import Data.Functor (($>))
import Data.List (singleton)
import Data.IORef

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  scope <- newIORef $ Scope [] [] [] [] Nothing

  let context = Context {
      contextScope = scope
    , contextQuads = core
    , contextIn = liftToSt getLine
    , contextOut = \str -> do
      liftToSt $ putStr str
      liftToSt $ hFlush stdout
    , contextErr = \str -> do
      liftToSt $ hPutStr stderr str
      liftToSt $ hFlush stderr }

  args <- getArgs
  case args of
    []     -> repl context
    [path] -> do
      code <- readFile path
      void $ runCode False path code context
    _      -> do
      hPutStrLn stderr "Usage:"
      hPutStrLn stderr "tinyapl         Start a REPL"
      hPutStrLn stderr "tinyapl path    Run a file"

runCode :: Bool -> String -> String -> Context -> IO Context
runCode output file code context = do
  result <- runResult $ run file code context
  case result of
    Left err -> hPrint stderr err $> context
    Right (res, context') -> if output then print res $> context' else return context'

repl :: Context -> IO ()
repl context = let
  go :: Context -> IO ()
  go context = do
    putStr "> "
    hFlush stdout
    line <- getLine
    if line == "" then return ()
    else runCode True "<repl>" line context >>= go
  in do
    putStrLn "TinyAPL REPL, empty line to exit"
    putStrLn "Supported primitives:"
    putStrLn $ "  " ++ unwords (singleton . fst <$> P.arrays)
    putStrLn $ "  " ++ unwords (singleton . fst <$> P.functions)
    putStrLn $ "  " ++ unwords (singleton . fst <$> P.adverbs)
    putStrLn $ "  " ++ unwords (singleton . fst <$> P.conjunctions)
    putStrLn "Supported quad names:"
    putStrLn $ "  " ++ unwords (fst <$> quadArrays core)
    putStrLn $ "  " ++ unwords (fst <$> quadFunctions core)
    putStrLn $ "  " ++ unwords (fst <$> quadAdverbs core)
    putStrLn $ "  " ++ unwords (fst <$> quadConjunctions core)
    putStrLn "Supported features:"
    putStrLn $ "* dfns " ++ [fst G.braces] ++ "code" ++ [snd G.braces] ++ ", d-monadic-ops " ++ [G.underscore, fst G.braces] ++ "code" ++ [snd G.braces] ++ ", d-dyadic-ops " ++ [G.underscore, fst G.braces] ++ "code" ++ [snd G.braces, G.underscore]
    putStrLn $ "  " ++ [G.alpha] ++ " left argument, " ++ [G.omega] ++ " right argument,"
    putStrLn $ "  " ++ [G.alpha, G.alpha] ++ " left array operand, " ++ [G.alphaBar, G.alphaBar] ++ " left function operand, " ++ [G.omega, G.omega] ++ " right array operand, " ++ [G.omegaBar, G.omegaBar] ++ " right function operand,"
    putStrLn $ "  " ++ [G.del] ++ " recurse function, " ++ [G.underscore, G.del] ++ " recurse monadic op, " ++ [G.underscore, G.del, G.underscore] ++ " recurse dyadic op"
    putStrLn $ "  " ++ [G.exit] ++ " early exit, " ++ [G.guard] ++ " guard"
    putStrLn $ "  " ++ [G.separator] ++ " multiple statements"
    putStrLn $ "* numbers: " ++ [G.decimal] ++ " decimal separator, " ++ [G.negative] ++ " negative sign, " ++ [G.exponent] ++ " exponent notation, " ++ [G.imaginary] ++ " complex separator"
    putStrLn $ "* character literals: " ++ [G.charDelimiter] ++ "abc" ++ [G.charDelimiter]
    putStrLn $ "* string literals: " ++ [G.stringDelimiter] ++ "abc" ++ [G.stringDelimiter] ++ " with escapes using " ++ [G.stringEscape]
    putStrLn $ "* names: abc array, Abc function, _Abc monadic op, _Abc_ dyadic op, assignment with " ++ [G.assign]
    putStrLn $ "* get " ++ [G.quad] ++ " read evaluated input, get " ++ [G.quadQuote] ++ " read string input, set " ++ [G.quad] ++ " print with newline, set " ++ [G.quadQuote] ++ " print without newline"
    putStrLn $ "* array notation: " ++ [fst G.vector, G.separator, snd G.vector] ++ " vector, " ++ [fst G.highRank, G.separator, snd G.highRank] ++ " higher rank array (combine major cells)"
    putStrLn $ "* trains: " ++ [fst G.train, snd G.train] ++ " deriving function, " ++ [G.underscore, fst G.train, snd G.train] ++ " deriving adverb, " ++ [G.underscore, fst G.train, snd G.train, G.underscore] ++ " deriving conjunction"
    putStrLn $ "* comments: " ++ [G.comment] ++ " until end of line, " ++ [fst G.inlineComment, snd G.inlineComment] ++ " inline"

    go context

