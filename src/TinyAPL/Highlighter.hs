module TinyAPL.Highlighter where

import qualified TinyAPL.Glyphs as G
import TinyAPL.Util

import Control.Monad.State
import Data.Char (chr)

data Color
  = COther
  | CSyntax
  | CNumber
  | CString
  | CStringEscape
  | CArrayName
  | CPrimArray
  | CFunctionName
  | CPrimFunction
  | CAdverbName
  | CPrimAdverb
  | CConjunctionName
  | CPrimConjunction
  | CComment
  deriving (Enum, Show)

data HState = HState
  { hColors :: [Color]
  , hStr :: String }

type HSt = State HState

highlight :: String -> [Color]
highlight str = reverse $ hColors $ execState hl (HState [] str) where
  atEnd :: HSt Bool
  atEnd = do
    str <- gets hStr
    pure $ null str
  
  andNotAtEnd :: HSt Bool -> HSt Bool
  andNotAtEnd p = do
    e <- atEnd
    if e then pure False else p

  peek :: HSt Char
  peek = do
    str <- gets hStr
    pure $ case str of (s:_) -> s; [] -> chr 0

  advance :: HSt Char
  advance = do
    st <- get
    let ss = hStr st
    case ss of
      [] -> pure $ chr 0
      (s:ss') -> do
        put $ st{ hStr = ss' }
        pure s

  push :: Color -> HSt ()
  push c = do
    st@HState{ hColors = cs } <- get
    put $ st{ hColors = c : cs }

  pushMany :: [Color] -> HSt ()
  pushMany = mapM_ push

  string :: HSt ()
  string = do
    start <- advance
    let isString = start == G.stringDelimiter
    push CString
    whileM_ (andNotAtEnd $ (/= start) <$> peek) $ do
      c <- peek
      if isString && c == G.stringEscape then do
        advance
        push CStringEscape
        advance
        push CStringEscape
      else return ()
      advance
      push CString
    advance
    push CString

  numberChars = ['0'..'9'] ++ [G.decimal, G.negative, G.exponent, G.imaginary, G.infinity]

  number :: HSt ()
  number = whileM_ (andNotAtEnd $ (`elem` numberChars) <$> peek) $ do
    advance
    push CNumber

  comment :: HSt ()
  comment = do
    advance
    push CComment
    whileM_ (andNotAtEnd $ (/= (snd G.inlineComment)) <$> peek) $ do
      advance
      push CComment
    advance
    push CComment

  identifierChars = G.underscore : ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ G.identifiers

  identifier :: HSt ()
  identifier = do
    is <- whileM (andNotAtEnd $ (`elem` identifierChars) <$> peek) advance
    if head is `elem` ['a'..'z'] ++ [G.alpha, G.omega, G.quad, G.quadQuote, G.delta] then pushMany $ const CArrayName <$> is
    else if head is `elem` ['A'..'Z'] ++ [G.alphaBar, G.omegaBar, G.deltaBar, G.del] then pushMany $ const CFunctionName <$> is
    else if head is == '_' && last is == '_' then pushMany $ const CConjunctionName <$> is
    else if head is == '_' then pushMany $ const CAdverbName <$> is
    else pushMany $ const COther <$> is

  hl :: HSt ()
  hl = whileM_ (not <$> atEnd) $ do
    p <- peek
    if p `elem` numberChars then number
    else if p `elem` [G.stringDelimiter, G.charDelimiter] then string
    else if p `elem` identifierChars then identifier
    else if p == fst G.inlineComment then comment
    else if p `elem` G.syntax then advance >> push CSyntax
    else if p `elem` G.arrays then advance >> push CPrimArray
    else if p `elem` G.functions then advance >> push CPrimFunction
    else if p `elem` G.adverbs then advance >> push CPrimAdverb
    else if p `elem` G.conjunctions then advance >> push CPrimConjunction
    else advance >> push COther
