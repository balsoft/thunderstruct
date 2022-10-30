{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Control.Applicative (some)
import Control.Monad (void)
import Cursor
import Data.Char (digitToInt)
import Data.Functor.Identity
import Text.Parsec hiding (Line)
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String
import Text.Parsec.Token
import Prelude hiding (Char, Word)

instance HasParser File where
  parser (CursorTreeLeaf {..}) = parse' (flip $ const (CursorTreeLeaf File)) (void $ many anyChar <* eof)
  parser (CursorTreeNode File n () (Identity l)) = parse' (CursorTreeNode File n) (manyTill (parser l) eof)



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

-- file :: CursorTree Identity File () -> Parser (CursorTree [] File (SourcePos, SourcePos))
-- file (CursorTreeLeaf {..}) = parse' (flip $ const (CursorTreeLeaf File)) (void $ many anyChar <* eof)
-- file (CursorTreeNode File _ () (Identity l)) = parse' (CursorTreeNode File FileOfLine) (manyTill (item l) eof)
-- file (CursorTreeNode File _ () (Identity w)) = parse' (CursorTreeNode FileOfWord) (manyTill (word w) eof)
-- file (CursorTreeNode File _ () (Identity c)) = parse' (CursorTreeNode FileOfChar) (manyTill (char' c) eof)
-- file (CursorTreeNode File _ () (Identity n)) = parse' (CursorTreeNode FileOfASTNode) (many $ node n)

-- item :: CursorTree Identity c () -> Parser (CursorTree [] c (SourcePos, SourcePos))
-- item l@(CursorTreeNode Line _ () _) = line l

orEof :: Parser a -> Parser ()
orEof p = try (void p) <|> eof

tilSep :: Parser p -> Parser s -> Parser [p]
tilSep p s = manyTill p (orEof s)

strSep = char '\n'

-- line :: CursorTree Identity Line () -> Parser (CursorTree [] Line (SourcePos, SourcePos))
-- line (CursorTreeLeaf {..}) = parse' (flip $ const CursorTreeLeaf) (void $ tilSep anyChar strSep)
-- line (CursorTreeNode Line LineOfWord () (Identity w)) = parse' (CursorTreeNode LineOfWord) (tilSep (word w) strSep)
-- line (CursorTreeNode Line LineOfChar () (Identity c)) = parse' (CursorTreeNode LineOfChar) (tilSep (char' c) strSep)

wordSep = some (oneOf "[]{}()., ")

-- word :: CursorTree Identity Word () -> Parser (CursorTree [] Word (SourcePos, SourcePos))
-- word (CursorTreeLeaf {..}) = parse' (flip $ const CursorTreeLeaf) (void $ tilSep anyChar wordSep)
-- word (CursorTreeNode Word WordOfChar () (Identity c)) = parse' (CursorTreeNode WordOfChar) (tilSep (char' c) wordSep)

-- char' :: CursorTree Identity Char () -> Parser (CursorTree [] Char (SourcePos, SourcePos))
-- char' (CursorTreeLeaf {..}) = parse' (flip $ const CursorTreeLeaf Char) (void anyChar)
-- char' (CursorTreeNode {}) = undefined -- GHC can't figure out that it's impossible (at least I think it's impossible?)

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

-- node' :: Parser (CursorTree [] ASTNode ((SourcePos, SourcePos), String))
-- node' =
--   do
--     from <- sourcePos
--     i <- getInput
--     nodef <-
--       (Parser.stringLiteral >> pure CursorTreeLeaf)
--         <|> (some (noneOf "(){}[]\n ") >> pure CursorTreeLeaf)
--         <|> flip (CursorTreeNode ASTNodeOfASTNode) <$> between (char '(') (char ')') (Parser.whiteSpace *> many node')
--         <|> flip (CursorTreeNode ASTNodeOfASTNode) <$> between (char '{') (char '}') (Parser.whiteSpace *> many node')
--         <|> flip (CursorTreeNode ASTNodeOfASTNode) <$> between (char '[') (char ']') (Parser.whiteSpace *> many node')
--     to <- sourcePos
--     i' <- getInput
--     Parser.whiteSpace
--     pure $ nodef ((from, to), take (length i - length i') i)

-- node :: CursorTree Identity ASTNode () -> Parser (CursorTree [] ASTNode (SourcePos, SourcePos))
-- node cur = traverseASTTree cur <$> node'
--   where
--     parseInContents p c s = case parse (many (p c) <* eof) "" s of
--       Right r -> r
--       Left _ -> []
--     traverseASTTree :: CursorTree Identity ASTNode () -> CursorTree [] ASTNode ((SourcePos, SourcePos), String) -> CursorTree [] ASTNode (SourcePos, SourcePos)
--     traverseASTTree (CursorTreeLeaf {}) (CursorTreeLeaf {..}) = CursorTreeLeaf ASTNode (fst contents)
--     traverseASTTree (CursorTreeLeaf {}) (CursorTreeNode {..}) = CursorTreeLeaf ASTNode (fst contents)
--     traverseASTTree (CursorTreeNode ASTNode ASTNodeOfASTNode _ _) (CursorTreeLeaf {..}) = CursorTreeNode ASTNode ASTNodeOfASTNode (fst contents) []
--     traverseASTTree (CursorTreeNode ASTNode ASTNodeOfASTNode _ (Identity cur')) (CursorTreeNode ASTNode ASTNodeOfASTNode contents children) = CursorTreeNode ASTNode ASTNodeOfASTNode (fst contents) (fmap (traverseASTTree cur') children)
--     traverseASTTree (CursorTreeNode ASTNode ASTNodeOfWord _ (Identity cur')) c = CursorTreeNode ASTNode ASTNodeOfWord (fst (contents c)) (parseInContents word cur' (snd (contents c)))
--     traverseASTTree (CursorTreeNode ASTNode ASTNodeOfChar _ (Identity cur')) c = CursorTreeNode ASTNode ASTNodeOfChar (fst (contents c)) (parseInContents char' cur' (snd (contents c)))
--     traverseASTTree (CursorTreeNode {}) (CursorTreeNode {}) = undefined -- node' only produces ASTNodeOfASTNode