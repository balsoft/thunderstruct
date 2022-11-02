{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parser where

import Control.Applicative (some)
import Control.Monad (void)
import Data.Char (digitToInt)
import Data.Functor.Identity
import Text.Parsec hiding (Line)
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String
import Text.Parsec.Token
import Prelude hiding (Char, Word)
import Numeric.Natural
import Util
import Data.Maybe
import Data.Foldable


data Char = Char deriving Show
data Word = Word deriving Show
data Line = Line deriving Show
data ASTNode = ASTNode deriving Show
data File = File deriving Show

class (Show a, Show b, HasParser a, HasParser b) => ConsistsOf a b
instance ConsistsOf Word Char
instance ConsistsOf Line Char
instance ConsistsOf Line Word
instance ConsistsOf ASTNode Char
instance ConsistsOf ASTNode Word
instance ConsistsOf ASTNode ASTNode
instance ConsistsOf File Char
instance ConsistsOf File Word
instance ConsistsOf File Line
instance ConsistsOf File ASTNode

class HasParser a where
  parser :: CursorTree Identity a () -> Parser (CursorTree [] a (SourcePos, SourcePos))

data CursorTree f t a where
  CursorTreeNode :: (ConsistsOf t t') => { _type :: t, contents :: a, children :: f (CursorTree f t' a) } -> CursorTree f t a
  CursorTreeLeaf :: (Show t) => { _type :: t, contents :: a } -> CursorTree f t a

a >--> b = CursorTreeNode a () (Identity b)
a >--| b = CursorTreeNode a () (Identity (CursorTreeLeaf b ()))

infixr 8 >-->

deriving instance Functor f => Functor (CursorTree f t)
deriving instance Foldable f => Foldable (CursorTree f t)
deriving instance Traversable f => Traversable (CursorTree f t)

instance (Show a, Foldable f) => Show (CursorTree f t a) where
  show (CursorTreeNode {..}) = show _type <> " " <> show contents <> childrens
    where
      fs = toList children
      childrens = case fs of
        [c] -> "→" <> show c
        _ -> " " <> show fs
  show (CursorTreeLeaf {..}) = show _type <> " " <> show contents

instance HasParser File where
  parser (CursorTreeLeaf {..}) = parse' (flip $ const (CursorTreeLeaf File)) (void $ many anyChar <* eof)
  parser (CursorTreeNode File () (Identity l)) = parse' (CursorTreeNode File) (many (parser l))

instance HasParser ASTNode where
  parser (CursorTreeLeaf {..}) = parse' (flip $ const (CursorTreeLeaf ASTNode)) (void node')
  parser (CursorTreeNode ASTNode () (Identity a)) = do
    i <- getInput
    from <- sourcePos
    node'
    to <- sourcePos
    i' <- getInput
    setInput (take (length i - length i') i)
    res <- some $ parser a
    setInput (drop (length i - length i') i)
    pure $ CursorTreeNode ASTNode (from, to) res


instance HasParser Line where
  parser (CursorTreeLeaf {..}) = parse' (flip $ const (CursorTreeLeaf Line)) (void $ tilSep anyChar strSep)
  parser (CursorTreeNode Line () (Identity l)) = parse' (CursorTreeNode Line) (tilSep (parser l) strSep)

instance HasParser Word where
  parser (CursorTreeLeaf {..}) = parse' (flip $ const (CursorTreeLeaf Word)) (void $ tilSep anyChar wordSep)
  parser (CursorTreeNode Word () (Identity w)) = parse' (CursorTreeNode Word) (tilSep (parser w) wordSep)

instance HasParser Char where
  parser (CursorTreeLeaf {..}) = parse' (flip $ const (CursorTreeLeaf Char)) (void anyChar)
  parser (CursorTreeNode Char () (Identity c)) = undefined

parseFile :: String -> CursorTree Identity File () -> Maybe (CursorTree [] File (SourcePos, SourcePos))
parseFile s f = case parse (parser f) "" s of
  Right r -> Just r
  Left _ -> Nothing

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `fmap` getParserState

parse' :: ((SourcePos, SourcePos) -> children -> self) -> Parser children -> Parser self
parse' f parser = do
  from <- sourcePos
  children <- parser
  to <- sourcePos
  pure $ f (from, to) children

orEof :: Parser a -> Parser ()
orEof p = void p <|> eof

parseIfNotContains :: Show b => Parser a -> Parser b -> Parser a
parseIfNotContains a b = do
  i <- getInput
  l <- lookAhead $ length <$> manyTill anyChar b
  setInput (take l i)
  res <- a
  setInput (drop l i)
  b
  pure res

tilSep :: Parser a -> Parser s -> Parser [a]
tilSep a sep = parseIfNotContains (some a) (void sep <|> eof)

takeWhileJust :: [Maybe a] -> [a]
takeWhileJust =
   foldr (\x acc -> maybe [] (:acc) x) []

strSep = char '\n'

wordSep = some (oneOf "[]{}().,\n ")

whiteSpace :: Parser ()
decimal :: Parser Integer

TokenParser {whiteSpace, decimal} = makeTokenParser haskellStyle

stringLiteral :: Parser String
stringLiteral =
  do
    str <-
      between
        (char '"')
        (char '"' <?> "end of string")
        (many stringChar)
    return (foldr (maybe id (:)) "" str)
    <?> "literal string"
  where
    stringChar =
      do Just <$> stringLetter
        <|> stringEscape
        <?> "string character"

    stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape = do
      _ <- char '\\'
      do _ <- escapeGap; return Nothing
        <|> do _ <- escapeEmpty; return Nothing
        <|> Just <$> escapeCode

    escapeEmpty = char '&'

    escapeGap = do
      _ <- many1 space
      char '\\' <?> "end of string gap"

    -- escape codes
    escapeCode =
      charEsc <|> charNum <|> charAscii <|> charControl
        <?> "escape code"

    charControl = do
      _ <- char '^'
      code <- upper
      return (toEnum (fromEnum code - fromEnum 'A' + 1))

    charNum = do
      code <-
        Parser.decimal
          <|> do _ <- char 'o'; number 8 octDigit
          <|> do _ <- char 'x'; number 16 hexDigit
      if code > 0x10FFFF
        then fail "invalid escape sequence"
        else return (toEnum (fromInteger code))

    number base baseDigit =
      do
        digits <- many1 baseDigit
        let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
        seq n (return n)

    charEsc = choice (map parseEsc escMap)
      where
        parseEsc (c, code) = do _ <- char c; return code

    charAscii = choice (map parseAscii asciiMap)
      where
        parseAscii (asc, code) = try (do _ <- string asc; return code)

    escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

    asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

    ascii2codes =
      [ "BS",
        "HT",
        "LF",
        "VT",
        "FF",
        "CR",
        "SO",
        "SI",
        "EM",
        "FS",
        "GS",
        "RS",
        "US",
        "SP"
      ]

    ascii3codes =
      [ "NUL",
        "SOH",
        "STX",
        "ETX",
        "EOT",
        "ENQ",
        "ACK",
        "BEL",
        "DLE",
        "DC1",
        "DC2",
        "DC3",
        "DC4",
        "NAK",
        "SYN",
        "ETB",
        "CAN",
        "SUB",
        "ESC",
        "DEL"
      ]

    ascii2 =
      [ '\BS',
        '\HT',
        '\LF',
        '\VT',
        '\FF',
        '\CR',
        '\SO',
        '\SI',
        '\EM',
        '\FS',
        '\GS',
        '\RS',
        '\US',
        '\SP'
      ]

    ascii3 =
      [ '\NUL',
        '\SOH',
        '\STX',
        '\ETX',
        '\EOT',
        '\ENQ',
        '\ACK',
        '\BEL',
        '\DLE',
        '\DC1',
        '\DC2',
        '\DC3',
        '\DC4',
        '\NAK',
        '\SYN',
        '\ETB',
        '\CAN',
        '\SUB',
        '\ESC',
        '\DEL'
      ]

-- parens :: Parser Node
-- parens = do
--   from <- sourcePos
--   parens' <- between
--           (oneOf "({[")
--           (oneOf ")}]")
--           (spaces *> (Parens <$> many node) <* spaces)
--   parens' from <$> sourcePos

node' :: Parser (CursorTree [] ASTNode (SourcePos, SourcePos))
node' =
  do
    from <- sourcePos
    i <- getInput
    nodef <-
      (Parser.stringLiteral >> pure (CursorTreeLeaf ASTNode))
        <|> (some (noneOf "(){}[]\n ") >> pure (CursorTreeLeaf ASTNode))
        <|> flip (CursorTreeNode ASTNode) <$> between (char '(') (char ')') (Parser.whiteSpace *> many node')
        <|> flip (CursorTreeNode ASTNode) <$> between (char '{') (char '}') (Parser.whiteSpace *> many node')
        <|> flip (CursorTreeNode ASTNode) <$> between (char '[') (char ']') (Parser.whiteSpace *> many node')
    to <- sourcePos
    i' <- getInput
    Parser.whiteSpace
    pure $ nodef (from, to)

data Selector a = None a | One Natural a |  Many [Natural] a | All a deriving (Functor, Foldable, Show, Eq)

select :: Selector [a] -> [a]
select (All a) = a
select (Many n a) = fmap snd $ filter ((`elem` n) . fst) $ zip [0..] a
select (One n a) = maybeToList (a ?+! n)
select (None _) = []

getCursorRange :: CursorTree Selector a () -> CursorTree [] b (SourcePos, SourcePos) -> [(SourcePos, SourcePos)]
getCursorRange (CursorTreeLeaf {}) (CursorTreeLeaf {..}) = [contents]
getCursorRange (CursorTreeLeaf {}) (CursorTreeNode {..}) = [contents]
getCursorRange (CursorTreeNode _ () s) (CursorTreeNode _ _ ns) = concat $ select $ fmap (\s' -> fmap (getCursorRange s') ns) s
getCursorRange (CursorTreeNode {}) (CursorTreeLeaf {}) = []