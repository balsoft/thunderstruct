{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module AST where

import Control.Applicative (Alternative (some))
import Data.Char (digitToInt)
import Data.List ()
import Numeric.Natural
import Text.Parsec
import Text.Parsec.Language (haskellStyle)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec.Token
import Util

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `fmap` getParserState

data Node = Parens [Node] Natural Natural | Token Natural Natural deriving (Show)

-- whiteSpace :: Parser ()
-- decimal :: Parser Integer

-- TokenParser {whiteSpace, decimal} = makeTokenParser haskellStyle

-- stringLiteral :: Parser String
-- stringLiteral =
--   do
--     str <-
--       between
--         (char '"')
--         (char '"' <?> "end of string")
--         (many stringChar)
--     return (foldr (maybe id (:)) "" str)
--     <?> "literal string"
--   where
--     stringChar =
--       do Just <$> stringLetter
--         <|> stringEscape
--         <?> "string character"

--     stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

--     stringEscape = do
--       _ <- char '\\'
--       do _ <- escapeGap; return Nothing
--         <|> do _ <- escapeEmpty; return Nothing
--         <|> Just <$> escapeCode

--     escapeEmpty = char '&'

--     escapeGap = do
--       _ <- many1 space
--       char '\\' <?> "end of string gap"

--     -- escape codes
--     escapeCode =
--       charEsc <|> charNum <|> charAscii <|> charControl
--         <?> "escape code"

--     charControl = do
--       _ <- char '^'
--       code <- upper
--       return (toEnum (fromEnum code - fromEnum 'A' + 1))

--     charNum = do
--       code <-
--         AST.decimal
--           <|> do _ <- char 'o'; number 8 octDigit
--           <|> do _ <- char 'x'; number 16 hexDigit
--       if code > 0x10FFFF
--         then fail "invalid escape sequence"
--         else return (toEnum (fromInteger code))

--     number base baseDigit =
--       do
--         digits <- many1 baseDigit
--         let n = foldl (\x d -> base * x + toInteger (digitToInt d)) 0 digits
--         seq n (return n)

--     charEsc = choice (map parseEsc escMap)
--       where
--         parseEsc (c, code) = do _ <- char c; return code

--     charAscii = choice (map parseAscii asciiMap)
--       where
--         parseAscii (asc, code) = try (do _ <- string asc; return code)

--     escMap = zip "abfnrtv\\\"\'" "\a\b\f\n\r\t\v\\\"\'"

--     asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

--     ascii2codes =
--       [ "BS",
--         "HT",
--         "LF",
--         "VT",
--         "FF",
--         "CR",
--         "SO",
--         "SI",
--         "EM",
--         "FS",
--         "GS",
--         "RS",
--         "US",
--         "SP"
--       ]

--     ascii3codes =
--       [ "NUL",
--         "SOH",
--         "STX",
--         "ETX",
--         "EOT",
--         "ENQ",
--         "ACK",
--         "BEL",
--         "DLE",
--         "DC1",
--         "DC2",
--         "DC3",
--         "DC4",
--         "NAK",
--         "SYN",
--         "ETB",
--         "CAN",
--         "SUB",
--         "ESC",
--         "DEL"
--       ]

--     ascii2 =
--       [ '\BS',
--         '\HT',
--         '\LF',
--         '\VT',
--         '\FF',
--         '\CR',
--         '\SO',
--         '\SI',
--         '\EM',
--         '\FS',
--         '\GS',
--         '\RS',
--         '\US',
--         '\SP'
--       ]

--     ascii3 =
--       [ '\NUL',
--         '\SOH',
--         '\STX',
--         '\ETX',
--         '\EOT',
--         '\ENQ',
--         '\ACK',
--         '\BEL',
--         '\DLE',
--         '\DC1',
--         '\DC2',
--         '\DC3',
--         '\DC4',
--         '\NAK',
--         '\SYN',
--         '\ETB',
--         '\CAN',
--         '\SUB',
--         '\ESC',
--         '\DEL'
--       ]

-- -- parens :: Parser Node
-- -- parens = do
-- --   from <- sourcePos
-- --   parens' <- between
-- --           (oneOf "({[")
-- --           (oneOf ")}]")
-- --           (spaces *> (Parens <$> many node) <* spaces)
-- --   parens' from <$> sourcePos

-- node :: Parser Node
-- node =
--   do
--     from <- sourcePos
--     nodef <-
--       Token <$> AST.stringLiteral
--         <|> Token <$> some (noneOf "(){}[]\n ")
--         <|> Parens <$> between (char '(') (char ')') (AST.whiteSpace *> many node)
--         <|> Parens <$> between (char '{') (char '}') (AST.whiteSpace *> many node)
--         <|> Parens <$> between (char '[') (char ']') (AST.whiteSpace *> many node)
--     to <- sourcePos
--     AST.whiteSpace
--     pure $ nodef from to

-- file :: Parser Node
-- file =
--   do
--     from <- sourcePos
--     nodes <- many (AST.whiteSpace *> node <* AST.whiteSpace)
--     to <- sourcePos
--     AST.whiteSpace
--     eof
--     pure $ Parens nodes from to

-- parseParens :: String -> Node
-- parseParens s = case parse file "" s of
--   Right result -> result
--   Left failure -> error (show failure)

-- parseParensE :: String -> Either ParseError Node
-- parseParensE = parse file ""

parensRanges :: Node -> [(Natural, Natural)]
parensRanges (Parens children pos len) = (pos, len) : concatMap parensRanges children
parensRanges (Token pos len) = [(pos, len)]

traverseAST :: [Natural] -> Node -> Either Node Node
traverseAST (i : is) node'@(Parens children _ _) = case children ?+! i of
  Just node'' -> traverseAST is node''
  Nothing -> Left node'
traverseAST (_ : _) node' = Left node'
traverseAST [] n = Right n

-- fromToToPosLen :: String -> SourcePos -> Natural
-- fromToToPosLen s from = fromIntegral $ lpos + fromIntegral (sourceColumn from) - 1
--   where
--     (lpos, _) = splitLines s !! (sourceLine from - 1)

-- fromToToPosLens :: String -> (SourcePos, SourcePos) -> (Natural, Natural)
-- fromToToPosLens s (from, to) = (c, c' - c)
--   where
--     c = fromToToPosLen s from
--     c' = fromToToPosLen s to
