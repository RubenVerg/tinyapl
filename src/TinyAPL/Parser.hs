module TinyAPL.Parser where

import qualified TinyAPL.Glyphs as G

import Text.Parsec
import Text.Parsec.String
import Data.Complex
import Data.Functor (($>))
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.List (singleton, elemIndex)
import Control.Applicative (liftA3)
import TinyAPL.Util (mapAdjacent, (.:))
import Data.Function (on)
import Data.Bifunctor (Bifunctor(bimap, first))
import Control.Monad ((>=>))

data Token
  = TokenNumber (Complex Double)
  | TokenCharVector String
  | TokenString String
  | TokenPrimArray Char
  | TokenPrimFunction Char
  | TokenPrimAdverb Char
  | TokenPrimConjunction Char
  | TokenDfn [[Token]]
  | TokenDadv [[Token]]
  | TokenDconj [[Token]]
  | TokenArrayName String
  | TokenFunctionName String
  | TokenAdverbName String
  | TokenConjunctionName String
  | TokenArrayAssign String [Token]
  | TokenFunctionAssign String [Token]
  | TokenAdverbAssign String [Token]
  | TokenConjunctionAssign String [Token]
  | TokenParens [Token]
  | TokenGuard [Token] [Token]
  | TokenExit [Token]
  deriving (Show)

tokenize :: SourceName -> String -> Either ParseError [Token]
tokenize = runParser (bits <* eof) () where
  arrayStart :: String
  arrayStart = G.delta : ['a'..'z']
  functionStart :: String
  functionStart = G.deltaBar : ['A'..'Z']
  identifierRest :: String
  identifierRest = arrayStart ++ functionStart ++ ['0'..'9']

  array :: Parser Token
  array = between spaces spaces (try number <|> try charVec <|> try string <|> try arrayAssign <|> try (TokenArrayName <$> arrayName) <|> primArray) where
    number :: Parser Token
    number = TokenNumber <$> complex where
      sign :: Parser Double
      sign = option 1 (char G.negative $> (-1))

      natural :: Parser String
      natural = many1 digit

      integer :: Parser (Double, String)
      integer = liftA2 (,) sign natural

      float :: Parser Double
      float = do
        (s, i) <- integer
        d <- option "" $ liftA2 (:) (char '.') (many1 digit)
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
    charVec = TokenCharVector <$> between (char G.charDelimiter) (char G.charDelimiter) (many $ noneOf ['\''])

    string :: Parser Token
    string = TokenString <$> between (char G.stringDelimiter) (char G.stringDelimiter) (many (escape <|> nonEscape)) where
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

    primArray :: Parser Token
    primArray = TokenPrimArray <$> oneOf G.arrays

    arrayName :: Parser String
    arrayName = singleton <$> oneOf [G.quad, G.quadQuote, G.alpha, G.omega] <|> liftA2 (:) (oneOf arrayStart) (many $ oneOf identifierRest)

    arrayAssign :: Parser Token
    arrayAssign = liftA2 TokenArrayAssign arrayName (char G.assign *> bits)

  function :: Parser Token
  function = between spaces spaces (try dfn <|> try functionAssign <|> try (TokenFunctionName <$> functionName) <|> primFunction) where
    dfn :: Parser Token
    dfn = TokenDfn <$> (string "{" *> sepBy1 definedBits separator <* string "}")

    primFunction :: Parser Token
    primFunction = TokenPrimFunction <$> oneOf G.functions

    functionName :: Parser String
    functionName = singleton <$> oneOf [G.alphaBar, G.omegaBar] <|> liftA2 (:) (oneOf functionStart) (many $ oneOf identifierRest)

    functionAssign :: Parser Token
    functionAssign = liftA2 TokenFunctionAssign functionName (char G.assign *> bits)

  adverb :: Parser Token
  adverb = between spaces spaces (try dadv <|> try adverbAssign <|> try (TokenAdverbName <$> adverbName) <|> primAdverb) where
    dadv :: Parser Token
    dadv = TokenDadv <$> (string "_{" *> sepBy1 definedBits separator <* string "}")

    primAdverb :: Parser Token
    primAdverb = TokenPrimAdverb <$> oneOf G.adverbs

    adverbName :: Parser String
    adverbName = liftA2 (:) (char '_') (many1 $ oneOf identifierRest)

    adverbAssign :: Parser Token
    adverbAssign = liftA2 TokenAdverbAssign adverbName (char G.assign *> bits)

  conjunction :: Parser Token
  conjunction = between spaces spaces (try dconj <|> try conjunctionAssign <|> try (TokenConjunctionName <$> conjunctionName) <|> primConjunction) where
    dconj :: Parser Token
    dconj = TokenDconj <$> (string "_{" *> sepBy1 definedBits separator <* string "}_")

    primConjunction :: Parser Token
    primConjunction = TokenPrimConjunction <$> oneOf G.conjunctions

    conjunctionName :: Parser String
    conjunctionName = liftA3 (\a b c -> a : b ++ [c]) (char '_') (many1 $ oneOf identifierRest) (char '_')

    conjunctionAssign :: Parser Token
    conjunctionAssign = liftA2 TokenConjunctionAssign conjunctionName (char G.assign *> bits)

  guard :: Parser Token
  guard = liftA2 TokenGuard bits (char G.guard *> bits)

  exit :: Parser Token
  exit = TokenExit <$> (char G.exit *> bits)

  bracketed :: Parser Token
  bracketed = TokenParens <$> between (char $ fst G.parens) (char $ snd G.parens) bits

  separator :: Parser ()
  separator = char G.separator $> ()

  bit :: Parser Token
  bit = bracketed <|> conjunction <|> adverb <|> function <|> array

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
  deriving (Enum, Bounded, Show, Eq, Ord)

data Tree
  = Leaf { leafCategory :: Category, leafToken :: Token }
  | Branch { branchCategory :: Category, branchLeft :: Tree, branchRight :: Tree }
  | AssignBranch { assignmentBranchCategory :: Category, assignmentName :: String, assignmentValue :: Tree }
  | DefinedBranch { definedBranchCategory :: Category, definedBranchStatements :: [Tree] }
  | GuardBranch { guardBranchCheck :: Tree, guardBranchResult :: Tree }
  | ExitBranch { exitBranchResult :: Tree }

instance Show Tree where
  show tree = unlines $ go 0 tree where
    indentCount = 2
    go :: Int -> Tree -> [String]
    go i t = let indent = replicate (indentCount * i) ' ' in case t of
      (Leaf c l) -> [indent ++ show c ++ ": " ++ show l]
      (Branch c l r) -> [indent ++ show c] ++ go (i + 1) l ++ go (i + 1) r
      (AssignBranch c n v) -> (indent ++ show c ++ " " ++ n ++ " ←") : go (i + 1) v
      (DefinedBranch c ts) -> (indent ++ show c ++ " {") : concatMap (go (i + 1)) ts
      (GuardBranch ch res) -> [indent ++ "guard"] ++ go (i + 1) ch ++ [indent ++ ":"] ++ go (i + 1) res
      (ExitBranch res) -> (indent ++ "■") : go (i + 1) res

treeCategory :: Tree -> Category
treeCategory (Leaf c _) = c
treeCategory (Branch c _ _) = c
treeCategory (AssignBranch c _ _) = c
treeCategory (DefinedBranch c _) = c
treeCategory (GuardBranch _ _) = CatArray
treeCategory (ExitBranch _) = CatArray

bindingMap :: [((Category, Category), (Int, Category))]
bindingMap =
  [ ((CatArray,           CatFunction), (2, CatAppliedFunction))
  , ((CatArray,           CatAdverb),   (3, CatFunction))
  , ((CatFunction,        CatArray),    (1, CatArray))
  , ((CatFunction,        CatAdverb),   (3, CatFunction))
  , ((CatAppliedFunction, CatArray),    (1, CatArray))
  , ((CatConjunction,     CatArray),    (3, CatAdverb))
  , ((CatConjunction,     CatFunction), (3, CatAdverb)) ]

pairs :: [Tree] -> [(Int, Category)]
pairs = mapAdjacent $ fromMaybe (0, minBound) .: (curry (`lookup` bindingMap) `on` treeCategory)

bindPair :: [Tree] -> Either String [Tree]
bindPair [] = Left "Bind empty array"
bindPair x@[_] = Right x
bindPair xs = let
  ps = pairs xs
  (sts, cats) = unzip ps
  maxBind = maximum sts
  nextBind = fromJust $ maxBind `elemIndex` sts
  category = cats !! nextBind
  indexed = zip [0..] xs
  in if maxBind == 0 then Left "No binding found" else Right $ mapMaybe (\(idx, el) ->
    if idx == nextBind then Just $ Branch category el $ xs !! (idx + 1)
    else if idx == nextBind + 1 then Nothing
    else Just el) indexed

bindAll :: [Tree] -> Either String Tree
bindAll [] = Left "Bind empty array"
bindAll [x] = Right x
bindAll xs = bindPair xs >>= bindAll

categorize :: SourceName -> String -> Either String [Tree]
categorize name = first show . tokenize name >=> categorizeTokens where
  categorizeTokens :: [Token] -> Either String [Tree]
  categorizeTokens = mapM tokenToTree

  categorizeAndBind :: [Token] -> Either String Tree
  categorizeAndBind = categorizeTokens >=> bindAll

  requireOfCategory :: Category -> (Category -> String) -> Tree -> Either String Tree
  requireOfCategory cat msg tree | treeCategory tree == cat = Right tree
                                 | otherwise                = Left $ msg $ treeCategory tree

  defined :: Category -> String -> [[Token]] -> Either String Tree
  defined cat name statements = do
    ss <- mapM categorizeAndBind statements
    if null ss then Left $ "Invalid empty " ++ name
    else if treeCategory (last ss) /= CatArray then Left $ "Invalid " ++ name ++ ": last statement must be an array"
    else Right $ DefinedBranch cat ss

  assignment :: Category -> String -> [Token] -> Either String Tree
  assignment cat name ts = AssignBranch cat name <$> (categorizeAndBind ts >>=
    requireOfCategory cat (\c -> "Invalid assignment of " ++ show c ++ " to " ++ show cat ++ " name"))

  tokenToTree :: Token -> Either String Tree
  tokenToTree num@(TokenNumber _) = return $ Leaf CatArray num
  tokenToTree ch@(TokenCharVector _) = return $ Leaf CatArray ch
  tokenToTree str@(TokenString _) = return $ Leaf CatArray str
  tokenToTree arr@(TokenPrimArray _) = return $ Leaf CatArray arr
  tokenToTree fn@(TokenPrimFunction _) = return $ Leaf CatFunction fn
  tokenToTree adv@(TokenPrimAdverb _) = return $ Leaf CatAdverb adv
  tokenToTree conj@(TokenPrimConjunction _) = return $ Leaf CatConjunction conj
  tokenToTree (TokenDfn statements) = defined CatFunction "dfn" statements
  tokenToTree (TokenDadv statements) = defined CatAdverb "dadv" statements
  tokenToTree (TokenDconj statements) = defined CatConjunction "dconj" statements
  tokenToTree arr@(TokenArrayName _) = return $ Leaf CatArray arr
  tokenToTree fn@(TokenFunctionName _) = return $ Leaf CatFunction fn
  tokenToTree adv@(TokenAdverbName _) = return $ Leaf CatAdverb adv
  tokenToTree conj@(TokenConjunctionName _) = return $ Leaf CatConjunction conj
  tokenToTree (TokenArrayAssign name ts) = assignment CatArray name ts
  tokenToTree (TokenFunctionAssign name ts) = assignment CatFunction name ts
  tokenToTree (TokenAdverbAssign name ts) = assignment CatAdverb name ts
  tokenToTree (TokenConjunctionAssign name ts) = assignment CatConjunction name ts
  tokenToTree (TokenParens ts) = categorizeAndBind ts
  tokenToTree (TokenGuard check result) = do
    c <- categorizeAndBind check >>= requireOfCategory CatArray (\c -> "Invalid guard of type " ++ show c ++ ", Array required")
    r <- categorizeAndBind result
    return $ GuardBranch c r
  tokenToTree (TokenExit result) = ExitBranch <$> (categorizeAndBind result >>= requireOfCategory CatArray (\c -> "Invalid exit statement of type " ++ show c ++ ", Array required"))
