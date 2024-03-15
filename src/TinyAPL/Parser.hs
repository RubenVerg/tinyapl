module TinyAPL.Parser where

import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import TinyAPL.Util

import Text.Parsec
import Text.Parsec.String
import Data.Complex
import Data.Functor (($>), void)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.List (elemIndex)
import Control.Applicative (liftA3, (<**>))
import Data.Function (on)
import Data.Bifunctor (Bifunctor(first))
import Control.Monad ((>=>))
import Text.Parsec.Error (errorMessages, showErrorMessages)
import Data.Char (isSpace)

data Token
  = TokenNumber (Complex Double) SourcePos
  | TokenChar String SourcePos
  | TokenString String SourcePos
  | TokenPrimArray Char SourcePos
  | TokenPrimFunction Char SourcePos
  | TokenPrimAdverb Char SourcePos
  | TokenPrimConjunction Char SourcePos
  | TokenDfn [[Token]] SourcePos
  | TokenDadv [[Token]] SourcePos
  | TokenDconj [[Token]] SourcePos
  | TokenArrayName String SourcePos
  | TokenFunctionName String SourcePos
  | TokenAdverbName String SourcePos
  | TokenConjunctionName String SourcePos
  | TokenArrayAssign String [Token] SourcePos
  | TokenFunctionAssign String [Token] SourcePos
  | TokenAdverbAssign String [Token] SourcePos
  | TokenConjunctionAssign String [Token] SourcePos
  | TokenParens [Token] SourcePos
  | TokenGuard [Token] [Token] SourcePos
  | TokenExit [Token] SourcePos
  | TokenVector [[Token]] SourcePos
  | TokenHighRank [[Token]] SourcePos
  deriving (Show)

tokenPos :: Token -> SourcePos
tokenPos (TokenNumber _ pos) = pos
tokenPos (TokenChar _ pos) = pos
tokenPos (TokenString _ pos) = pos
tokenPos (TokenPrimArray _ pos) = pos
tokenPos (TokenPrimFunction _ pos) = pos
tokenPos (TokenPrimAdverb _ pos) = pos
tokenPos (TokenPrimConjunction _ pos) = pos
tokenPos (TokenDfn _ pos) = pos
tokenPos (TokenDadv _ pos) = pos
tokenPos (TokenDconj _ pos) = pos
tokenPos (TokenArrayName _ pos) = pos
tokenPos (TokenFunctionName _ pos) = pos
tokenPos (TokenAdverbName _ pos) = pos
tokenPos (TokenConjunctionName _ pos) = pos
tokenPos (TokenArrayAssign _ _ pos) = pos
tokenPos (TokenFunctionAssign _ _ pos) = pos
tokenPos (TokenAdverbAssign _ _ pos) = pos
tokenPos (TokenConjunctionAssign _ _ pos) = pos
tokenPos (TokenParens _ pos) = pos
tokenPos (TokenGuard _ _ pos) = pos
tokenPos (TokenExit _ pos) = pos
tokenPos (TokenVector _ pos) = pos
tokenPos (TokenHighRank _ pos) = pos

makeSyntaxError :: SourcePos -> String -> String -> Error
makeSyntaxError pos source msg = let
  line = sourceLine pos
  column = sourceColumn pos
  theLine = if null $ lines source then "" else lines source !! (line - 1)
  in SyntaxError $ theLine ++ "\n" ++ replicate (column - 1) ' ' ++ "^\n" ++ msg

makeParseError :: String -> ParseError -> Error
makeParseError source err = let
  pos = errorPos err
  msgs = tail $ showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" $ errorMessages err
  in makeSyntaxError pos source msgs

tokenize :: SourceName -> String -> Result [[Token]]
tokenize file source = first (makeParseError source) $ Text.Parsec.parse (sepBy1 bits separator <* eof) file source where
  withPos :: Parser (SourcePos -> a) -> Parser a
  withPos = (<**>) getPosition

  arrayStart :: String
  arrayStart = G.delta : ['a'..'z']
  functionStart :: String
  functionStart = G.deltaBar : ['A'..'Z']
  identifierRest :: String
  identifierRest = arrayStart ++ functionStart ++ ['0'..'9']

  whitespace :: Parser ()
  whitespace = do
    let comm :: Parser ()
        comm = char (fst G.inlineComment) *> void (many $ noneOf [snd G.inlineComment]) <* char (snd G.inlineComment)
    void $ many $ try comm <|> void (satisfy isSpace)

  array :: Parser Token
  array = between whitespace whitespace (try number <|> try charVec <|> try str <|> try arrayAssign <|> try (withPos $ TokenArrayName <$> arrayName) <|> try vectorNotation <|> try highRankNotation <|> primArray) where
    number :: Parser Token
    number = withPos $ TokenNumber <$> complex where
      sign :: Parser Double
      sign = option 1 (char G.negative $> (-1))

      natural :: Parser String
      natural = many1 digit

      integer :: Parser (Double, String)
      integer = liftA2 (,) sign natural

      float :: Parser Double
      float = do
        (s, i) <- integer
        d <- option "" $ liftA2 (:) (char G.decimal) (many1 digit)
        return $ s * read (i ++ d)

      scientific :: Parser Double
      scientific = do
        f <- float
        option f $ do
          _ <- char G.exponent
          (es, ei) <- integer
          return $ f * 10 ** (es * read ei)

      complex :: Parser (Complex Double)
      complex = liftA2 (:+) scientific (option 0 (char G.imaginary *> scientific))

    charVec :: Parser Token
    charVec = withPos $ TokenChar <$> between (char G.charDelimiter) (char G.charDelimiter) (many $ noneOf ['\''])

    str :: Parser Token
    str = withPos $ TokenString <$> between (char G.stringDelimiter) (char G.stringDelimiter) (many (escape <|> nonEscape)) where
      escape :: Parser Char
      escape = do
        _ <- char G.stringEscape
        c <- oneOf [G.stringDelimiter, G.stringEscape, 'n', 'r', 't']
        return $ fromJust $ lookup c
          [ (G.stringDelimiter, G.stringDelimiter)
          , (G.stringEscape, G.stringEscape)
          , ('n', '\n')
          , ('r', '\r')
          , ('t', '\t') ]

      nonEscape :: Parser Char
      nonEscape = noneOf [G.stringDelimiter, G.stringEscape]

    vectorNotation :: Parser Token
    vectorNotation = withPos $ between (char $ fst G.vector) (char $ snd G.vector) (TokenVector <$> sepBy bits separator)

    highRankNotation :: Parser Token
    highRankNotation = withPos $ between (char $ fst G.highRank) (char $ snd G.highRank) (TokenHighRank <$> sepBy bits separator)

    primArray :: Parser Token
    primArray = withPos $ TokenPrimArray <$> oneOf G.arrays

    arrayName :: Parser String
    arrayName = try (string [G.alpha, G.alpha]) <|> try (string [G.omega, G.omega]) <|> try (string [G.alpha]) <|> try (string [G.omega]) <|> try (string [G.quad]) <|> try (string [G.quadQuote]) <|> liftA2 (:) (oneOf arrayStart) (many $ oneOf identifierRest)

    arrayAssign :: Parser Token
    arrayAssign = withPos $ liftA2 TokenArrayAssign arrayName (between whitespace whitespace (char G.assign) *> bits)

  function :: Parser Token
  function = between whitespace whitespace (try dfn <|> try functionAssign <|> try (withPos $ TokenFunctionName <$> functionName) <|> primFunction) where
    dfn :: Parser Token
    dfn = withPos $ TokenDfn <$> (string [fst G.braces] *> sepBy1 definedBits separator <* string [snd G.braces])

    primFunction :: Parser Token
    primFunction = withPos $ TokenPrimFunction <$> oneOf G.functions

    functionName :: Parser String
    functionName = try (string [G.del]) <|> try (string [G.alphaBar, G.alphaBar]) <|> try (string [G.omegaBar, G.omegaBar]) <|> liftA2 (:) (oneOf functionStart) (many $ oneOf identifierRest)

    functionAssign :: Parser Token
    functionAssign = withPos $ liftA2 TokenFunctionAssign functionName (between whitespace whitespace (char G.assign) *> bits)

  adverb :: Parser Token
  adverb = between whitespace whitespace (try dadv <|> try adverbAssign <|> try (withPos $ TokenAdverbName <$> adverbName) <|> primAdverb) where
    dadv :: Parser Token
    dadv = withPos $ TokenDadv <$> (string [G.underscore, fst G.braces] *> sepBy1 definedBits separator <* string [snd G.braces])

    primAdverb :: Parser Token
    primAdverb = withPos $ TokenPrimAdverb <$> oneOf G.adverbs

    adverbName :: Parser String
    adverbName = try (string [G.underscore, G.del]) <|> liftA2 (:) (char G.underscore) (many1 $ oneOf identifierRest)

    adverbAssign :: Parser Token
    adverbAssign = withPos $ liftA2 TokenAdverbAssign adverbName (between whitespace whitespace (char G.assign) *> bits)

  conjunction :: Parser Token
  conjunction = between whitespace whitespace (try dconj <|> try conjunctionAssign <|> try (withPos $ TokenConjunctionName <$> conjunctionName) <|> primConjunction) where
    dconj :: Parser Token
    dconj = withPos $ TokenDconj <$> (string [G.underscore, fst G.braces] *> sepBy1 definedBits separator <* string [snd G.braces, G.underscore])

    primConjunction :: Parser Token
    primConjunction = withPos $ TokenPrimConjunction <$> oneOf G.conjunctions

    conjunctionName :: Parser String
    conjunctionName = try (string [G.underscore, G.del, G.underscore]) <|> liftA3 (\a b c -> a : b ++ [c]) (char G.underscore) (many1 $ oneOf identifierRest) (char G.underscore)

    conjunctionAssign :: Parser Token
    conjunctionAssign = withPos $ liftA2 TokenConjunctionAssign conjunctionName (between whitespace whitespace (char G.assign) *> bits)

  guard :: Parser Token
  guard = withPos $ liftA2 TokenGuard bits (char G.guard *> definedBits)

  exit :: Parser Token
  exit = withPos $ TokenExit <$> (char G.exit *> bits)

  bracketed :: Parser Token
  bracketed = withPos $ TokenParens <$> between (char $ fst G.parens) (char $ snd G.parens) bits

  separator :: Parser ()
  separator = oneOf [G.separator, '\n'] $> ()

  bit :: Parser Token
  bit = try bracketed <|> try conjunction <|> try adverb <|> try function <|> try array

  bits :: Parser [Token]
  bits = many1 bit

  definedBits :: Parser [Token]
  definedBits = many1 (try guard <|> try exit <|> bit)

data Category
  = CatArray
  | CatFunction
  | CatAppliedFunction
  | CatAdverb
  | CatConjunction
  deriving (Enum, Bounded, Eq, Ord)

instance Show Category where
  show CatArray           = "array"
  show CatFunction        = "function"
  show CatAppliedFunction = "applied function"
  show CatAdverb          = "monadic operator"
  show CatConjunction     = "dyadic operator"

data Tree
  = Leaf { leafCategory :: Category, leafToken :: Token }
  | MonadCallBranch { monadCallBranchLeft :: Tree, monadCallBranchRight :: Tree }
  | DyadCallBranch { dyadCallBranchLeft :: Tree, dyadCallBranchRight :: Tree }
  | AdverbCallBranch { adverbCallBranchLeft :: Tree, adverbCallBranchRight :: Tree }
  | ConjunctionCallBranch { conjunctionCallBranchLeft :: Tree, conjunctionCallBranchRight :: Tree }
  | AssignBranch { assignmentBranchCategory :: Category, assignmentName :: String, assignmentValue :: Tree }
  | DefinedBranch { definedBranchCategory :: Category, definedBranchStatements :: [Tree] }
  | GuardBranch { guardBranchCheck :: Tree, guardBranchResult :: Tree }
  | ExitBranch { exitBranchResult :: Tree }
  | VectorBranch { vectorEntries :: [Tree] }
  | HighRankBranch { highRankEntries :: [Tree] }

instance Show Tree where
  show tree = unlines $ go 0 tree where
    indentCount = 2
    go :: Int -> Tree -> [String]
    go i t = let indent = replicate (indentCount * i) ' ' in case t of
      (Leaf c l)                  -> [indent ++ show c ++ ": " ++ show l]
      (MonadCallBranch l r)       -> [indent ++ "monad call"] ++ go (i + 1) l ++ go (i + 1) r
      (DyadCallBranch l r)        -> [indent ++ "dyad left call"] ++ go (i + 1) l ++ go (i + 1) r
      (AdverbCallBranch l r)      -> [indent ++ "adverb call"] ++ go (i + 1) l ++ go (i + 1) r
      (ConjunctionCallBranch l r) -> [indent ++ "conjunction right call"] ++ go (i + 1) l ++ go (i + 1) r
      (AssignBranch c n v)        -> (indent ++ show c ++ " " ++ n ++ " ←") : go (i + 1) v
      (DefinedBranch c ts)        -> (indent ++ show c ++ " {") : concatMap (go (i + 1)) ts
      (GuardBranch ch res)        -> [indent ++ "guard"] ++ go (i + 1) ch ++ [indent ++ ":"] ++ go (i + 1) res
      (ExitBranch res)            -> (indent ++ "■") : go (i + 1) res
      (VectorBranch es)           -> (indent ++ "⟨⟩") : concatMap (go (i + 1)) es
      (HighRankBranch es)         -> (indent ++ "[]") : concatMap (go (i + 1)) es

treeCategory :: Tree -> Category
treeCategory (Leaf c _)                  = c
treeCategory (MonadCallBranch _ _)       = CatArray
treeCategory (DyadCallBranch _ _)        = CatAppliedFunction
treeCategory (AdverbCallBranch _ _)      = CatFunction
treeCategory (ConjunctionCallBranch _ _) = CatAdverb
treeCategory (AssignBranch c _ _)        = c
treeCategory (DefinedBranch c _)         = c
treeCategory (GuardBranch _ _)           = CatArray
treeCategory (ExitBranch _)              = CatArray
treeCategory (VectorBranch _)            = CatArray
treeCategory (HighRankBranch _)          = CatArray

bindingMap :: [((Category, Category), (Int, Tree -> Tree -> Tree))]
bindingMap =
  [ ((CatArray,           CatFunction), (2, DyadCallBranch))
  , ((CatFunction,        CatArray),    (1, MonadCallBranch))
  , ((CatAppliedFunction, CatArray),    (1, MonadCallBranch))
  , ((CatFunction,        CatAdverb),   (3, AdverbCallBranch))
  , ((CatArray,           CatAdverb),   (3, AdverbCallBranch))
  , ((CatConjunction,     CatArray),    (3, ConjunctionCallBranch))
  , ((CatConjunction,     CatFunction), (3, ConjunctionCallBranch)) ]

pairs :: [Tree] -> [(Int, Tree -> Tree -> Tree)]
pairs = mapAdjacent $ fromMaybe (0, undefined) .: (curry (`lookup` bindingMap) `on` treeCategory)

bindPair :: [Tree] -> Result [Tree]
bindPair [] = err $ SyntaxError "Bind empty array"
bindPair x@[_] = pure x
bindPair xs = let
  (sts, trees) = unzip $ pairs xs
  maxBind = maximum sts
  nextBind = fromJust $ maxBind `elemIndex` sts
  tree = trees !! nextBind
  indexed = zip [0..] xs
  in if maxBind == 0 then err $ SyntaxError "No binding found" else pure $ mapMaybe (\(idx, el) ->
    if idx == nextBind then Just $ tree el $ xs !! (idx + 1)
    else if idx == nextBind + 1 then Nothing
    else Just el) indexed

bindAll :: [Tree] -> Result Tree
bindAll [] = err $ SyntaxError "Bind empty array"
bindAll [x] = pure x
bindAll xs = bindPair xs >>= bindAll

categorize :: SourceName -> String -> Result [[Tree]]
categorize name source = tokenize name source >>= mapM categorizeTokens where
  categorizeTokens :: [Token] -> Result [Tree]
  categorizeTokens = mapM tokenToTree

  categorizeAndBind :: [Token] -> Result Tree
  categorizeAndBind = categorizeTokens >=> bindAll

  requireOfCategory :: Category -> (Category -> Error) -> Tree -> Result Tree
  requireOfCategory cat msg tree | treeCategory tree == cat = pure tree
                                 | otherwise                = err $ msg $ treeCategory tree

  defined :: Category -> String -> [[Token]] -> SourcePos -> Result Tree
  defined cat name statements pos = do
    ss <- mapM categorizeAndBind statements
    if null ss then err $ makeSyntaxError pos source $ "Invalid empty " ++ name
    else if treeCategory (last ss) /= CatArray then err $ makeSyntaxError (tokenPos $ head $ last statements) source $ "Invalid " ++ name ++ ": last statement must be an array"
    else Right $ DefinedBranch cat ss

  assignment :: Category -> String -> [Token] -> SourcePos -> Result Tree
  assignment cat name ts pos = AssignBranch cat name <$> (categorizeAndBind ts >>=
    requireOfCategory cat (\c -> makeSyntaxError pos source $ "Invalid assignment of " ++ show c ++ " to " ++ show cat ++ " name"))

  array :: ([Tree] -> Tree) -> [[Token]] -> SourcePos -> Result Tree
  array t es _ = t <$> mapM (\x -> categorizeAndBind x >>=
    requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ head x) source $ "Invalid array entry of type " ++ show c ++ ", array required")) es

  tokenToTree :: Token -> Result Tree
  tokenToTree num@(TokenNumber _ _)                = return $ Leaf CatArray num
  tokenToTree ch@(TokenChar _ _)                   = return $ Leaf CatArray ch
  tokenToTree str@(TokenString _ _)                = return $ Leaf CatArray str
  tokenToTree arr@(TokenPrimArray _ _)             = return $ Leaf CatArray arr
  tokenToTree fn@(TokenPrimFunction _ _)           = return $ Leaf CatFunction fn
  tokenToTree adv@(TokenPrimAdverb _ _)            = return $ Leaf CatAdverb adv
  tokenToTree conj@(TokenPrimConjunction _ _)      = return $ Leaf CatConjunction conj
  tokenToTree (TokenDfn statements pos)            = defined CatFunction "dfn" statements pos
  tokenToTree (TokenDadv statements pos)           = defined CatAdverb "dadv" statements pos
  tokenToTree (TokenDconj statements pos)          = defined CatConjunction "dconj" statements pos
  tokenToTree arr@(TokenArrayName _ _)             = return $ Leaf CatArray arr
  tokenToTree fn@(TokenFunctionName _ _)           = return $ Leaf CatFunction fn
  tokenToTree adv@(TokenAdverbName _ _)            = return $ Leaf CatAdverb adv
  tokenToTree conj@(TokenConjunctionName _ _)      = return $ Leaf CatConjunction conj
  tokenToTree (TokenArrayAssign name ts pos)       = assignment CatArray name ts pos
  tokenToTree (TokenFunctionAssign name ts pos)    = assignment CatFunction name ts pos
  tokenToTree (TokenAdverbAssign name ts pos)      = assignment CatAdverb name ts pos
  tokenToTree (TokenConjunctionAssign name ts pos) = assignment CatConjunction name ts pos
  tokenToTree (TokenParens ts _)                   = categorizeAndBind ts
  tokenToTree (TokenGuard check result _)          = do
    c <- categorizeAndBind check >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ head check) source $ "Invalid guard of type " ++ show c ++ ", array required")
    r <- categorizeAndBind result
    return $ GuardBranch c r
  tokenToTree (TokenExit result _)                 = ExitBranch <$> (categorizeAndBind result >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ head result) source $ "Invalid exit statement of type " ++ show c ++ ", array required"))
  tokenToTree (TokenVector es pos)                 = array VectorBranch es pos
  tokenToTree (TokenHighRank es pos)               = array HighRankBranch es pos

parse :: SourceName -> String -> Result [Tree]
parse name = categorize name >=> mapM bindAll
