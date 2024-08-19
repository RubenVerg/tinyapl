{-# LANGUAGE LambdaCase, BangPatterns #-}

module TinyAPL.Parser where

import TinyAPL.Complex
import TinyAPL.Error
import qualified TinyAPL.Glyphs as G
import TinyAPL.Util

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Functor (($>), void)
import Data.Maybe (fromJust, fromMaybe, mapMaybe)
import Data.List (elemIndex, intercalate)
import Control.Applicative (liftA3, (<**>))
import Data.Function (on)
import Data.Bifunctor (Bifunctor(first))
import Control.Monad ((>=>))
import Data.Void (Void)
import Text.Parser.Combinators (sepByNonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

type Parser = Parsec Void String

data Token
  = TokenNumber [(Complex Double)] SourcePos
  | TokenChar String SourcePos
  | TokenString String SourcePos
  | TokenPrimArray Char SourcePos
  | TokenPrimFunction Char SourcePos
  | TokenPrimAdverb Char SourcePos
  | TokenPrimConjunction Char SourcePos
  | TokenDfn (NonEmpty (NonEmpty Token)) SourcePos
  | TokenDadv (NonEmpty (NonEmpty Token)) SourcePos
  | TokenDconj (NonEmpty (NonEmpty Token)) SourcePos
  | TokenArrayName String SourcePos
  | TokenFunctionName String SourcePos
  | TokenAdverbName String SourcePos
  | TokenConjunctionName String SourcePos
  | TokenQualifiedArrayName Token (NonEmpty String) SourcePos
  | TokenQualifiedFunctionName Token (NonEmpty String) SourcePos
  | TokenQualifiedAdverbName Token (NonEmpty String) SourcePos
  | TokenQualifiedConjunctionName Token (NonEmpty String) SourcePos
  | TokenArrayAssign String (NonEmpty Token) SourcePos
  | TokenFunctionAssign String (NonEmpty Token) SourcePos
  | TokenAdverbAssign String (NonEmpty Token) SourcePos
  | TokenConjunctionAssign String (NonEmpty Token) SourcePos
  | TokenQualifiedArrayAssign Token (NonEmpty String) (NonEmpty Token) SourcePos
  | TokenQualifiedFunctionAssign Token (NonEmpty String) (NonEmpty Token) SourcePos
  | TokenQualifiedAdverbAssign Token (NonEmpty String) (NonEmpty Token) SourcePos
  | TokenQualifiedConjunctionAssign Token (NonEmpty String) (NonEmpty Token) SourcePos
  | TokenVectorAssign [String] (NonEmpty Token) SourcePos
  | TokenHighRankAssign [String] (NonEmpty Token) SourcePos
  | TokenParens (NonEmpty Token) SourcePos
  | TokenGuard (NonEmpty Token) (NonEmpty Token) SourcePos
  | TokenExit (NonEmpty Token) SourcePos
  | TokenVector [NonEmpty Token] SourcePos
  | TokenHighRank [NonEmpty Token] SourcePos
  | TokenTrain [[Token]] SourcePos
  | TokenAdverbTrain [[Token]] SourcePos
  | TokenConjunctionTrain [[Token]] SourcePos
  | TokenWrap (NonEmpty Token) SourcePos
  | TokenUnwrap (NonEmpty Token) SourcePos
  | TokenStruct [NonEmpty Token] SourcePos

instance Eq Token where
  (TokenNumber x _) == (TokenNumber y _) = x == y
  (TokenChar x _) == (TokenChar y _) = x == y
  (TokenString x _) == (TokenString y _) = x == y
  (TokenPrimArray x _) == (TokenPrimArray y _) = x == y
  (TokenPrimFunction x _) == (TokenPrimFunction y _) = x == y
  (TokenPrimAdverb x _) == (TokenPrimAdverb y _) = x == y
  (TokenPrimConjunction x _) == (TokenPrimConjunction y _) = x == y
  (TokenDfn x _) == (TokenDfn y _) = x == y
  (TokenDadv x _) == (TokenDadv y _) = x == y
  (TokenDconj x _) == (TokenDconj y _) = x == y
  (TokenArrayName x _) == (TokenArrayName y _) = x == y
  (TokenFunctionName x _) == (TokenFunctionName y _) = x == y
  (TokenAdverbName x _) == (TokenAdverbName y _) = x == y
  (TokenConjunctionName x _) == (TokenConjunctionName y _) = x == y
  (TokenQualifiedArrayName x _ _) == (TokenQualifiedArrayName y _ _) = x == y
  (TokenQualifiedFunctionName x _ _) == (TokenQualifiedFunctionName y _ _) = x == y
  (TokenQualifiedAdverbName x _ _) == (TokenQualifiedAdverbName y _ _) = x == y
  (TokenQualifiedConjunctionName x _ _) == (TokenQualifiedConjunctionName y _ _) = x == y
  (TokenArrayAssign xn x _) == (TokenArrayAssign yn y _) = xn == yn && x == y
  (TokenFunctionAssign xn x _) == (TokenFunctionAssign yn y _) = xn == yn && x == y
  (TokenAdverbAssign xn x _) == (TokenAdverbAssign yn y _) = xn == yn && x == y
  (TokenConjunctionAssign xn x _) == (TokenConjunctionAssign yn y _) = xn == yn && x == y
  (TokenQualifiedArrayAssign xh xs xv _) == (TokenQualifiedArrayAssign yh ys yv _) = xh == yh && xs == ys && xv == yv
  (TokenQualifiedFunctionAssign xh xs xv _) == (TokenQualifiedFunctionAssign yh ys yv _) = xh == yh && xs == ys && xv == yv
  (TokenQualifiedAdverbAssign xh xs xv _) == (TokenQualifiedAdverbAssign yh ys yv _) = xh == yh && xs == ys && xv == yv
  (TokenQualifiedConjunctionAssign xh xs xv _) == (TokenQualifiedConjunctionAssign yh ys yv _) = xh == yh && xs == ys && xv == yv
  (TokenVectorAssign xn x _) == (TokenVectorAssign yn y _) = xn == yn && x == y
  (TokenHighRankAssign xn x _) == (TokenHighRankAssign yn y _) = xn == yn && x == y
  (TokenParens x _) == (TokenParens y _) = x == y
  (TokenGuard xc xe _) == (TokenGuard yc ye _) = xc == yc && xe == ye
  (TokenExit x _) == (TokenExit y _) = x == y
  (TokenVector x _) == (TokenVector y _) = x == y
  (TokenHighRank x _) == (TokenHighRank y _) = x == y
  (TokenTrain x _) == (TokenTrain y _) = x == y
  (TokenAdverbTrain x _) == (TokenAdverbTrain y _) = x == y
  (TokenConjunctionTrain x _) == (TokenConjunctionTrain y _) = x == y
  (TokenWrap x _) == (TokenWrap y _) = x == y
  (TokenUnwrap x _) == (TokenUnwrap y _) = x == y
  (TokenStruct x _) == (TokenStruct y _) = x == y
  _ == _ = False

instance Show Token where
  show (TokenNumber x _) = "(number " ++ show x ++ ")"
  show (TokenChar x _) = "(character " ++ [G.charDelimiter] ++ x ++ [G.charDelimiter] ++ ")"
  show (TokenString x _) = "(string " ++ [G.stringDelimiter] ++ x ++ [G.stringDelimiter] ++ ")"
  show (TokenPrimArray x _) = "(primitive array " ++ [x] ++ ")"
  show (TokenPrimFunction x _) = "(primitive function " ++ [x] ++ ")"
  show (TokenPrimAdverb x _) = "(primitive adverb " ++ [x] ++ ")"
  show (TokenPrimConjunction x _) = "(primitive conjunction " ++ [x] ++ ")"
  show (TokenDfn xs _) = "(dfn " ++ [fst G.braces, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . NE.toList . fmap show <$> NE.toList xs) ++ [' ', snd G.braces] ++ ")"
  show (TokenDadv xs _) = "(dadv " ++ [G.underscore, fst G.braces, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . NE.toList . fmap show <$> NE.toList xs) ++ [' ', snd G.braces] ++ ")"
  show (TokenDconj xs _) = "(dconj " ++ [G.underscore, fst G.braces, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . NE.toList . fmap show <$> NE.toList xs) ++ [' ', snd G.braces, G.underscore] ++ ")"
  show (TokenArrayName x _) = "(array name " ++ x ++ ")"
  show (TokenFunctionName x _) = "(function name " ++ x ++ ")"
  show (TokenAdverbName x _) = "(adverb name " ++ x ++ ")"
  show (TokenConjunctionName x _) = "(conjunction name " ++ x ++ ")"
  show (TokenQualifiedArrayName t ns _) = "(qualified array name " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ ")"
  show (TokenQualifiedFunctionName t ns _) = "(qualified function name " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ ")"
  show (TokenQualifiedAdverbName t ns _) = "(qualified adverb name " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ ")"
  show (TokenQualifiedConjunctionName t ns _) = "(qualified conjunction name " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ ")"
  show (TokenArrayAssign x xs _) = "(array assign " ++ x ++ [' ', G.assign, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenFunctionAssign x xs _) = "(function assign " ++ x ++ [' ', G.assign, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenAdverbAssign x xs _) = "(adverb assign " ++ x ++ [' ', G.assign, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenConjunctionAssign x xs _) = "(conjunction assign " ++ x ++ [' ', G.assign, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenQualifiedArrayAssign t ns xs _) = "(qualified array assign " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ [' ', G.assign, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenQualifiedFunctionAssign t ns xs _) = "(qualified function assign " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ [' ', G.assign, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenQualifiedAdverbAssign t ns xs _) = "(qualified adverb assign " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ [' ', G.assign, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenQualifiedConjunctionAssign t ns xs _) = "(qualified conjunction assign " ++ show t ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ [' ', G.assign, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenVectorAssign ns xs _) = "(vector assign " ++ unwords (show <$> ns) ++ " " ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenHighRankAssign ns xs _) = "(high rank assign " ++ unwords (show <$> ns) ++ " " ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenParens xs _) = "(parens (" ++ unwords (NE.toList $ show <$> xs) ++ "))"
  show (TokenGuard gs rs _) = "(guard " ++ unwords (NE.toList $ show <$> gs) ++ " : " ++ unwords (NE.toList $ show <$> rs) ++ ")"
  show (TokenExit xs _) = "(exit " ++ [G.exit, ' '] ++ unwords (NE.toList $ show <$> xs) ++ ")"
  show (TokenVector xs _) = "(vector " ++ [fst G.vector, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . NE.toList . fmap show <$> xs) ++ [snd G.vector] ++ ")"
  show (TokenHighRank xs _) = "(high rank " ++ [fst G.highRank, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . NE.toList . fmap show <$> xs) ++ [snd G.highRank] ++ ")"
  show (TokenTrain xs _) = "(train " ++ [fst G.train, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . fmap show <$> xs) ++ [snd G.train] ++ ")" where
  show (TokenAdverbTrain xs _) = "(adverb train " ++ [G.underscore, fst G.train, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . fmap show <$> xs) ++ [snd G.train] ++ ")" where
  show (TokenConjunctionTrain xs _) = "(conjunction train " ++ [G.underscore, fst G.train, ' '] ++ intercalate [' ', G.separator, ' '] (unwords . fmap show <$> xs) ++ [snd G.train, G.underscore] ++ ")" where
  show (TokenWrap xs _) = "(wrap " ++ [fst G.parens, G.wrap, ' '] ++ unwords (NE.toList $ show <$> xs) ++ [snd G.parens] ++ ")"
  show (TokenUnwrap xs _) = "(unwrap " ++ [fst G.parens, G.unwrap, ' '] ++ unwords (NE.toList $ show <$> xs) ++ [snd G.parens] ++ ")"
  show (TokenStruct xs _) = "(struct " ++ [fst G.struct] ++ intercalate [' ', G.separator, ' '] (unwords . NE.toList . fmap show <$> xs) ++ [snd G.struct] ++ ")"

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
tokenPos (TokenQualifiedArrayName _ _ pos) = pos
tokenPos (TokenQualifiedFunctionName _ _ pos) = pos
tokenPos (TokenQualifiedAdverbName _ _ pos) = pos
tokenPos (TokenQualifiedConjunctionName _ _ pos) = pos
tokenPos (TokenArrayAssign _ _ pos) = pos
tokenPos (TokenFunctionAssign _ _ pos) = pos
tokenPos (TokenAdverbAssign _ _ pos) = pos
tokenPos (TokenConjunctionAssign _ _ pos) = pos
tokenPos (TokenQualifiedArrayAssign _ _ _ pos) = pos
tokenPos (TokenQualifiedFunctionAssign _ _ _ pos) = pos
tokenPos (TokenQualifiedAdverbAssign _ _ _ pos) = pos
tokenPos (TokenQualifiedConjunctionAssign _ _ _ pos) = pos
tokenPos (TokenVectorAssign _ _ pos) = pos
tokenPos (TokenHighRankAssign _ _ pos) = pos
tokenPos (TokenParens _ pos) = pos
tokenPos (TokenGuard _ _ pos) = pos
tokenPos (TokenExit _ pos) = pos
tokenPos (TokenVector _ pos) = pos
tokenPos (TokenHighRank _ pos) = pos
tokenPos (TokenTrain _ pos) = pos
tokenPos (TokenAdverbTrain _ pos) = pos
tokenPos (TokenConjunctionTrain _ pos) = pos
tokenPos (TokenWrap _ pos) = pos
tokenPos (TokenUnwrap _ pos) = pos
tokenPos (TokenStruct _ pos) = pos

emptyPos :: SourcePos
emptyPos = SourcePos "<empty>" (mkPos 1) (mkPos 1)

prettyError :: SourcePos -> String -> String
prettyError pos source = let
  ls = lines source
  line = subtract 1 $ unPos $ sourceLine pos
  column = subtract 1 $ unPos $ sourceColumn pos
  theLine = if length ls <= line then "" else ls !! line
  in theLine ++ "\n" ++ replicate column ' ' ++ "^\n"

prettyParseError :: String -> SourcePos -> ParseError String Void -> String
prettyParseError source pos err = prettyError pos source ++ parseErrorTextPretty err

makeSyntaxError :: SourcePos -> String -> String -> Error
makeSyntaxError pos source msg = SyntaxError $ prettyError pos source ++ msg ++ "\n"

makeParseErrors :: String -> ParseErrorBundle String Void -> Error
makeParseErrors source es = case attachSourcePos errorOffset (bundleErrors es) (bundlePosState es) of
  (r :| rs, _) -> SyntaxError $ concatMap (uncurry $ flip $ prettyParseError source) $ r : rs

tokenize :: String -> String -> Result [NonEmpty Token]
tokenize file source = first (makeParseErrors source) $ Text.Megaparsec.parse (sepBy1 bits separator <* eof) file source where
  withPos :: Parser (SourcePos -> a) -> Parser a
  withPos = (<**>) getSourcePos

  spaceConsumer :: Parser ()
  spaceConsumer = L.space space1 (L.skipLineComment [G.comment]) (L.skipBlockComment [fst G.inlineComment] [snd G.inlineComment])

  lexeme :: Parser a -> Parser a
  lexeme = L.lexeme spaceConsumer

  commitOn :: Parser a -> Parser b -> Parser a
  commitOn p q = try (p <* lookAhead q) <* q

  commitOn' :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  commitOn' f p q = liftA2 f (try $ p <* lookAhead q) q

  arrayStart :: String
  arrayStart = G.delta : ['a'..'z']

  functionStart :: String
  functionStart = G.deltaBar : ['A'..'Z']

  identifierRest :: String
  identifierRest = arrayStart ++ functionStart ++ ['0'..'9']

  assign :: (a -> NonEmpty Token -> SourcePos -> b) -> Parser a -> Parser b
  assign con name = withPos $ liftA2 con (lexeme name `commitOn` char G.assign) bits

  arrayName :: Parser String
  arrayName = try (liftA3 (\x y z -> x : y : z) (char G.quad) (oneOf arrayStart) (many $ oneOf identifierRest)) <|> try (string [G.alpha, G.alpha]) <|> try (string [G.omega, G.omega]) <|> try (string [G.alpha]) <|> try (string [G.omega]) <|> try (string [G.quad]) <|> try (string [G.quadQuote]) <|> liftA2 (:) (oneOf arrayStart) (many $ oneOf identifierRest)

  functionName :: Parser String
  functionName = try (liftA3 (\x y z -> x : y : z) (char G.quad) (oneOf functionStart) (many $ oneOf identifierRest)) <|> try (string [G.del]) <|> try (string [G.alphaBar, G.alphaBar]) <|> try (string [G.omegaBar, G.omegaBar]) <|> liftA2 (:) (oneOf functionStart) (many $ oneOf identifierRest)

  adverbName :: Parser String
  adverbName = try (liftA3 (\x y z -> x : y : z) (char G.quad) (char G.underscore) (many $ oneOf identifierRest)) <|> try (string [G.underscore, G.del]) <|> liftA2 (:) (char G.underscore) (some $ oneOf identifierRest)

  conjunctionName :: Parser String
  conjunctionName = try ((\x y z w -> x : y : z ++ [w]) <$> char G.quad <*> char G.underscore <*> many (oneOf identifierRest) <*> char G.underscore) <|> try (string [G.underscore, G.del, G.underscore]) <|> liftA3 (\a b c -> a : b ++ [c]) (char G.underscore) (some $ oneOf identifierRest) (char G.underscore)

  qualified :: Show a => (a -> NonEmpty String -> b) -> Parser a -> Parser String -> Parser String -> Parser b
  qualified make head piece tail = do
    ((first, middle), last) <- commitOn' (,) (liftA2 (,) (head `commitOn` char G.access) (many $ lexeme piece `commitOn` char G.access)) (lexeme tail)
    pure $ make first $ snocNE middle last

  arrayAssign :: Parser Token
  arrayAssign = assign TokenArrayAssign arrayName

  functionAssign :: Parser Token
  functionAssign = assign TokenFunctionAssign functionName

  adverbAssign :: Parser Token
  adverbAssign = assign TokenAdverbAssign adverbName

  conjunctionAssign :: Parser Token
  conjunctionAssign = assign TokenConjunctionAssign conjunctionName

  vectorAssign :: Parser Token
  vectorAssign = assign TokenVectorAssign $ between (char $ fst G.vector) (char $ snd G.vector) (sepBy arrayName separator)

  highRankAssign :: Parser Token
  highRankAssign = assign TokenHighRankAssign $ between (char $ fst G.highRank) (char $ snd G.highRank) (sepBy arrayName separator)

  array' :: Parser Token
  array' = number <|> charVec <|> str <|> try (withPos $ TokenArrayName <$> arrayName) <|> vectorNotation <|> highRankNotation <|> primArray <|> wrap <|> struct where
    number :: Parser Token
    number = withPos $ TokenNumber <$> sepBy1 complex (lexeme $ char G.tie) where
      sign :: Parser Double
      sign = option 1 (char G.negative $> (-1))

      natural :: Parser String
      natural = some digitChar

      integer :: Parser (Double, String)
      integer = liftA2 (,) sign natural

      float :: Parser Double
      float = do
        (s, i) <- integer
        d <- option "" $ liftA2 (:) (char G.decimal) natural
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
        return $ fromJust $ lookup c G.escapes

      nonEscape :: Parser Char
      nonEscape = noneOf [G.stringDelimiter, G.stringEscape]

    vectorNotation :: Parser Token
    vectorNotation = withPos $ between (char $ fst G.vector) (char $ snd G.vector) (TokenVector <$> sepBy bits separator)

    highRankNotation :: Parser Token
    highRankNotation = withPos $ between (char $ fst G.highRank) (char $ snd G.highRank) (TokenHighRank <$> sepBy bits separator)

    primArray :: Parser Token
    primArray = withPos $ TokenPrimArray <$> oneOf G.arrays

    wrap :: Parser Token
    wrap = withPos $ TokenWrap <$> (string [fst G.parens, G.wrap] *> bits <* string [snd G.parens])

    struct :: Parser Token
    struct = withPos $ TokenStruct <$> (string [fst G.struct] *> sepBy bits separator <* string [snd G.struct])

  array :: Parser Token
  array = withPos (liftA2 ($) (qualified TokenQualifiedArrayAssign bit' arrayName arrayName `commitOn` char G.assign) bits)
    <|> withPos (qualified TokenQualifiedArrayName bit' arrayName arrayName) <|> vectorAssign <|> highRankAssign <|> arrayAssign <|> array'

  function' :: Parser Token
  function' = dfn <|> train <|> try (withPos $ TokenFunctionName <$> functionName) <|> primFunction <|> unwrap where
    dfn :: Parser Token
    dfn = withPos $ TokenDfn <$> (string [fst G.braces] *> sepByNonEmpty definedBits separator <* string [snd G.braces])

    train :: Parser Token
    train = withPos $ TokenTrain <$> (string [fst G.train] *> sepBy1 bitsMaybe separator <* string [snd G.train])

    primFunction :: Parser Token
    primFunction = withPos $ TokenPrimFunction <$> oneOf G.functions

    unwrap :: Parser Token
    unwrap = withPos $ TokenUnwrap <$> (string [fst G.parens, G.unwrap] *> bits <* string [snd G.parens])

  function :: Parser Token
  function = withPos (liftA2 ($) (qualified TokenQualifiedFunctionAssign bit' arrayName functionName `commitOn` char G.assign) bits) <|> withPos (qualified TokenQualifiedFunctionName bit' arrayName functionName) <|> functionAssign <|> function'

  adverb' :: Parser Token
  adverb' = try dadv <|> try adverbTrain <|> try (withPos $ TokenAdverbName <$> adverbName) <|> primAdverb where
    dadv :: Parser Token
    dadv = withPos $ TokenDadv <$> (string [G.underscore, fst G.braces] *> sepByNonEmpty definedBits separator <* string [snd G.braces] <* notFollowedBy (char G.underscore))

    adverbTrain :: Parser Token
    adverbTrain = withPos $ TokenAdverbTrain <$> (string [G.underscore, fst G.train] *> sepBy1 bitsMaybe separator <* string [snd G.train] <* notFollowedBy (char G.underscore))

    primAdverb :: Parser Token
    primAdverb = withPos $ TokenPrimAdverb <$> oneOf G.adverbs

  adverb :: Parser Token
  adverb = withPos (liftA2 ($) (qualified TokenQualifiedAdverbAssign bit' arrayName adverbName `commitOn` char G.assign) bits) <|> withPos (qualified TokenQualifiedAdverbName bit' arrayName adverbName) <|> adverbAssign <|> adverb'

  conjunction' :: Parser Token
  conjunction' = try dconj <|> try conjunctionTrain <|> try (withPos $ TokenConjunctionName <$> conjunctionName) <|> primConjunction where
    dconj :: Parser Token
    dconj = withPos $ TokenDconj <$> (string [G.underscore, fst G.braces] *> sepByNonEmpty definedBits separator <* string [snd G.braces, G.underscore])

    conjunctionTrain :: Parser Token
    conjunctionTrain = withPos $ TokenConjunctionTrain <$> (string [G.underscore, fst G.train] *> sepBy1 bitsMaybe separator <* string [snd G.train, G.underscore])

    primConjunction :: Parser Token
    primConjunction = withPos $ TokenPrimConjunction <$> oneOf G.conjunctions

  conjunction :: Parser Token
  conjunction = withPos (liftA2 ($) (qualified TokenQualifiedConjunctionAssign bit' arrayName conjunctionName `commitOn` char G.assign) bits) <|> withPos (qualified TokenQualifiedConjunctionName bit' arrayName conjunctionName) <|> conjunctionAssign <|> conjunction'

  guard :: Parser Token
  guard = withPos $ liftA2 TokenGuard (bits `commitOn` char G.guard) definedBits

  exit :: Parser Token
  exit = withPos $ TokenExit <$> (char G.exit *> bits)

  bracketed :: Parser Token
  bracketed = withPos $ TokenParens <$> between (char $ fst G.parens) (char $ snd G.parens) bits

  separator :: Parser ()
  separator = void $ char (G.separator)

  bit' :: Parser Token
  bit' = lexeme $ conjunction' <|> adverb' <|> function' <|> array' <|> bracketed

  bit :: Parser Token
  bit = lexeme $ conjunction <|> adverb <|> function <|> array <|> bracketed

  bitsMaybe :: Parser [Token]
  bitsMaybe = spaceConsumer *> many bit

  bits :: Parser (NonEmpty Token)
  bits = spaceConsumer *> NE.some1 bit

  definedBits :: Parser (NonEmpty Token)
  definedBits = spaceConsumer *> NE.some1 (lexeme guard <|> lexeme exit <|> bit)

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
  | QualifiedBranch { qualifiedBranchCategory :: Category, qualifiedBranchHead :: Tree, qualifiedBranchNames :: NonEmpty String }
  | MonadCallBranch { monadCallBranchLeft :: Tree, monadCallBranchRight :: Tree }
  | DyadCallBranch { dyadCallBranchLeft :: Tree, dyadCallBranchRight :: Tree }
  | AdverbCallBranch { adverbCallBranchLeft :: Tree, adverbCallBranchRight :: Tree }
  | ConjunctionCallBranch { conjunctionCallBranchLeft :: Tree, conjunctionCallBranchRight :: Tree }
  | AssignBranch { assignmentBranchCategory :: Category, assignmentName :: String, assignmentValue :: Tree }
  | QualifiedAssignBranch { qualifiedAssignBranchCategory :: Category, qualifiedAssignBranchHead :: Tree, qualifiedAssignBranchNames :: NonEmpty String, qualifiedAssignBranchValue :: Tree }
  | VectorAssignBranch { vectorAssignBranchNames :: [String], vectorAssignBranchValue :: Tree }
  | HighRankAssignBranch { highRankAssignBranchNames :: [String], highRankAssignBranchValue :: Tree }
  | DefinedBranch { definedBranchCategory :: Category, definedBranchStatements :: NonEmpty Tree }
  | GuardBranch { guardBranchCheck :: Tree, guardBranchResult :: Tree }
  | ExitBranch { exitBranchResult :: Tree }
  | VectorBranch { vectorBranchEntries :: [Tree] }
  | HighRankBranch { highRankBranchEntries :: [Tree] }
  | TrainBranch { trainBranchCategory :: Category, trainBranchStatements :: [Maybe Tree] }
  | WrapBranch { wrapBranchValue :: Tree }
  | UnwrapBranch { unwrapBranchValue :: Tree }
  | StructBranch { structBranchStatements :: [Tree] }
  deriving (Eq)

instance Show Tree where
  show tree = unlines $ go 0 tree where
    indentCount = 2
    go :: Int -> Tree -> [String]
    go i t = let indent = replicate (indentCount * i) ' ' in case t of
      (Leaf c l)                       -> [indent ++ show c ++ ": " ++ show l]
      (QualifiedBranch c h ns)         -> [indent ++ show c ++ ": ..." ++ [G.access] ++ intercalate [G.access] (NE.toList ns)] ++ go (i + 1) h
      (MonadCallBranch l r)            -> [indent ++ "monad call"] ++ go (i + 1) l ++ go (i + 1) r
      (DyadCallBranch l r)             -> [indent ++ "dyad left call"] ++ go (i + 1) l ++ go (i + 1) r
      (AdverbCallBranch l r)           -> [indent ++ "adverb call"] ++ go (i + 1) l ++ go (i + 1) r
      (ConjunctionCallBranch l r)      -> [indent ++ "conjunction right call"] ++ go (i + 1) l ++ go (i + 1) r
      (AssignBranch c n v)             -> (indent ++ show c ++ " " ++ n ++ " ←") : go (i + 1) v
      (QualifiedAssignBranch c h ns v) -> (indent ++ show c ++ " ..." ++ [G.access] ++ intercalate [G.access] (NE.toList ns) ++ " ← ...") : go (i + 1) h ++ (indent ++ "←") : go (i + 1) v
      (VectorAssignBranch ns v)        -> (indent ++ "⟨⟩ " ++ unwords (show <$> ns) ++ " ←") : go (i + 1) v
      (HighRankAssignBranch ns v)      -> (indent ++ "[] " ++ unwords (show <$> ns) ++ " ←") : go (i + 1) v
      (DefinedBranch c ts)             -> (indent ++ show c ++ " {}") : concatMap (go (i + 1)) ts
      (GuardBranch ch res)             -> [indent ++ "guard"] ++ go (i + 1) ch ++ [indent ++ ":"] ++ go (i + 1) res
      (ExitBranch res)                 -> (indent ++ "■") : go (i + 1) res
      (VectorBranch es)                -> (indent ++ "⟨⟩") : concatMap (go (i + 1)) es
      (HighRankBranch es)              -> (indent ++ "[]") : concatMap (go (i + 1)) es
      (TrainBranch c ts)               -> (indent ++ (if c == CatFunction then "" else "_") ++ "⦅" ++ (if c == CatConjunction then "_" else "") ++ "⦆") : concatMap (maybe [""] (go (i + 1))) ts
      (WrapBranch fn)                  -> (indent ++ "□") : go (i + 1) fn
      (UnwrapBranch fn)                -> (indent ++ "⊏") : go (i + 1) fn
      (StructBranch ts)                -> (indent ++ "⦃⦄") : concatMap (go (i + 1)) ts

treeCategory :: Tree -> Category
treeCategory (Leaf c _)                      = c
treeCategory (QualifiedBranch c _ _)         = c
treeCategory (MonadCallBranch _ _)           = CatArray
treeCategory (DyadCallBranch _ _)            = CatAppliedFunction
treeCategory (AdverbCallBranch _ _)          = CatFunction
treeCategory (ConjunctionCallBranch _ _)     = CatAdverb
treeCategory (AssignBranch c _ _)            = c
treeCategory (QualifiedAssignBranch c _ _ _) = c
treeCategory (VectorAssignBranch _ _)        = CatArray
treeCategory (HighRankAssignBranch _ _)      = CatArray
treeCategory (DefinedBranch c _)             = c
treeCategory (GuardBranch _ t)               = treeCategory t
treeCategory (ExitBranch _)                  = CatArray
treeCategory (VectorBranch _)                = CatArray
treeCategory (HighRankBranch _)              = CatArray
treeCategory (TrainBranch c _)               = c
treeCategory (WrapBranch _)                  = CatArray
treeCategory (UnwrapBranch _)                = CatFunction
treeCategory (StructBranch _)                = CatArray

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

bindPair :: NonEmpty Tree -> Result (NonEmpty Tree)
bindPair x@(_ :| []) = pure x
bindPair xs = let
  xs' = NE.toList xs
  (sts, trees) = unzip $ pairs xs'
  maxBind = maximum sts
  nextBind = fromJust $ maxBind `elemIndex` sts
  tree = trees !! nextBind
  indexed = zip [0..] xs'
  in if maxBind == 0 then throwError $ SyntaxError "No binding found" else pure $ NE.fromList $ mapMaybe (\(idx, el) ->
    if idx == nextBind then Just $ tree el $ xs' !! (idx + 1)
    else if idx == nextBind + 1 then Nothing
    else Just el) indexed

bindAll :: NonEmpty Tree -> Result Tree
bindAll (x :| []) = pure x
bindAll xs = bindPair xs >>= bindAll

categorize :: String -> String -> Result [NonEmpty Tree]
categorize name source = tokenize name source >>= mapM categorizeTokens where
  categorizeTokens :: NonEmpty Token -> Result (NonEmpty Tree)
  categorizeTokens = mapM tokenToTree

  categorizeAndBind :: NonEmpty Token -> Result Tree
  categorizeAndBind = categorizeTokens >=> bindAll

  requireOfCategory :: Category -> (Category -> Error) -> Tree -> Result Tree
  requireOfCategory cat msg tree | treeCategory tree == cat = pure tree
                                 | otherwise                = throwError $ msg $ treeCategory tree

  qualified :: Category -> Token -> NonEmpty String -> Result Tree
  qualified cat h ns = QualifiedBranch cat <$> (tokenToTree h >>=
    requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos h) source $ "Invalid qualified access to value of type " ++ show c)) <*> pure ns

  defined :: Category -> String -> NonEmpty (NonEmpty Token) -> SourcePos -> Result Tree
  defined cat name statements pos = do
    ss <- mapM categorizeAndBind statements
    if null ss then throwError $ makeSyntaxError pos source $ "Invalid empty " ++ name
    else if treeCategory (NE.last ss) /= CatArray then throwError $ makeSyntaxError (tokenPos $ NE.head $ NE.last statements) source $ "Invalid " ++ name ++ ": last statement must be an array"
    else Right $ DefinedBranch cat ss

  assignment :: Category -> String -> NonEmpty Token -> SourcePos -> Result Tree
  assignment cat name ts pos = AssignBranch cat name <$> (categorizeAndBind ts >>=
    requireOfCategory cat (\c -> makeSyntaxError pos source $ "Invalid assignment of " ++ show c ++ " to " ++ show cat ++ " name"))

  qualifiedAssignment :: Category -> Token -> NonEmpty String -> NonEmpty Token -> Result Tree
  qualifiedAssignment cat h ns ts = liftA2 (\h' as -> QualifiedAssignBranch cat h' ns as) (tokenToTree h >>=
    requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos h) source $ "Invalid qualified access to value of type " ++ show c)) (categorizeAndBind ts >>=
    requireOfCategory cat (\c -> makeSyntaxError (tokenPos $ NE.head ts) source $ "Invalid assignment of " ++ show c ++ " to " ++ show cat ++ " name"))

  destructureAssignment :: ([String] -> Tree -> Tree) -> [String] -> NonEmpty Token -> SourcePos -> Result Tree
  destructureAssignment h names ts pos = h names <$> (categorizeAndBind ts >>= requireOfCategory CatArray (\c -> makeSyntaxError pos source $ "Invalid destructure assignment of " ++ show c ++ ", array required"))

  vector :: [NonEmpty Token] -> SourcePos -> Result Tree
  vector es _ = VectorBranch <$> mapM (\x -> categorizeAndBind x >>= \x' -> case treeCategory x' of
    CatArray -> pure x'
    CatFunction -> pure x'
    _ -> throwError $ makeSyntaxError (tokenPos $ NE.head x) source $ "Invalid vector entry of type " ++ show (treeCategory x') ++ ", array or function required") es

  highRank :: [NonEmpty Token] -> SourcePos -> Result Tree
  highRank es _ = HighRankBranch <$> mapM (\x -> categorizeAndBind x >>=
    requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head x) source $ "Invalid array entry of type " ++ show c ++ ", array required")) es

  train :: Category -> [[Token]] -> SourcePos -> Result Tree
  train cat es _ = TrainBranch cat <$> (mapM (\e -> case NE.nonEmpty e of
    Nothing -> return Nothing
    Just e' -> Just <$> categorizeAndBind e') es)

  struct :: [NonEmpty Token] -> SourcePos -> Result Tree
  struct es _ = StructBranch <$> mapM (\x -> categorizeAndBind x) es

  tokenToTree :: Token -> Result Tree
  tokenToTree num@(TokenNumber _ _)                       = return $ Leaf CatArray num
  tokenToTree ch@(TokenChar _ _)                          = return $ Leaf CatArray ch
  tokenToTree str@(TokenString _ _)                       = return $ Leaf CatArray str
  tokenToTree arr@(TokenPrimArray _ _)                    = return $ Leaf CatArray arr
  tokenToTree fn@(TokenPrimFunction _ _)                  = return $ Leaf CatFunction fn
  tokenToTree adv@(TokenPrimAdverb _ _)                   = return $ Leaf CatAdverb adv
  tokenToTree conj@(TokenPrimConjunction _ _)             = return $ Leaf CatConjunction conj
  tokenToTree (TokenDfn statements pos)                   = defined CatFunction "dfn" statements pos
  tokenToTree (TokenDadv statements pos)                  = defined CatAdverb "dadv" statements pos
  tokenToTree (TokenDconj statements pos)                 = defined CatConjunction "dconj" statements pos
  tokenToTree arr@(TokenArrayName _ _)                    = return $ Leaf CatArray arr
  tokenToTree fn@(TokenFunctionName _ _)                  = return $ Leaf CatFunction fn
  tokenToTree adv@(TokenAdverbName _ _)                   = return $ Leaf CatAdverb adv
  tokenToTree conj@(TokenConjunctionName _ _)             = return $ Leaf CatConjunction conj
  tokenToTree (TokenQualifiedArrayName h ns _)            = qualified CatArray h ns
  tokenToTree (TokenQualifiedFunctionName h ns _)         = qualified CatFunction h ns
  tokenToTree (TokenQualifiedAdverbName h ns _)           = qualified CatAdverb h ns
  tokenToTree (TokenQualifiedConjunctionName h ns _)      = qualified CatConjunction h ns
  tokenToTree (TokenArrayAssign name ts pos)              = assignment CatArray name ts pos
  tokenToTree (TokenFunctionAssign name ts pos)           = assignment CatFunction name ts pos
  tokenToTree (TokenAdverbAssign name ts pos)             = assignment CatAdverb name ts pos
  tokenToTree (TokenConjunctionAssign name ts pos)        = assignment CatConjunction name ts pos
  tokenToTree (TokenQualifiedArrayAssign h ns ts _)       = qualifiedAssignment CatArray h ns ts
  tokenToTree (TokenQualifiedFunctionAssign h ns ts _)    = qualifiedAssignment CatFunction h ns ts
  tokenToTree (TokenQualifiedAdverbAssign h ns ts _)      = qualifiedAssignment CatAdverb h ns ts
  tokenToTree (TokenQualifiedConjunctionAssign h ns ts _) = qualifiedAssignment CatConjunction h ns ts
  tokenToTree (TokenVectorAssign names ts pos)            = destructureAssignment VectorAssignBranch names ts pos
  tokenToTree (TokenHighRankAssign names ts pos)          = destructureAssignment HighRankAssignBranch names ts pos
  tokenToTree (TokenParens ts _)                          = categorizeAndBind ts
  tokenToTree (TokenGuard check result _)                 = liftA2 GuardBranch (categorizeAndBind check >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head check) source $ "Invalid guard of type " ++ show c ++ ", array required")) (categorizeAndBind result)
  tokenToTree (TokenExit result _)                        = ExitBranch <$> (categorizeAndBind result >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head result) source $ "Invalid exit statement of type " ++ show c ++ ", array required"))
  tokenToTree (TokenVector es pos)                        = vector es pos
  tokenToTree (TokenHighRank es pos)                      = highRank es pos
  tokenToTree (TokenTrain fs pos)                         = train CatFunction fs pos
  tokenToTree (TokenAdverbTrain fs pos)                   = train CatAdverb fs pos
  tokenToTree (TokenConjunctionTrain fs pos)              = train CatConjunction fs pos
  tokenToTree (TokenWrap val _)                           = WrapBranch <$> (categorizeAndBind val >>= requireOfCategory CatFunction (\c -> makeSyntaxError (tokenPos $ NE.head val) source $ "Invalid wrap of type " ++ show c ++ ", function required"))
  tokenToTree (TokenUnwrap val _)                         = UnwrapBranch <$> (categorizeAndBind val >>= requireOfCategory CatArray (\c -> makeSyntaxError (tokenPos $ NE.head val) source $ "Invalid unwrap of type " ++ show c ++ ", array required"))
  tokenToTree (TokenStruct es pos)                        = struct es pos

parse :: String -> String -> Result [Tree]
parse name = categorize name >=> mapM bindAll
