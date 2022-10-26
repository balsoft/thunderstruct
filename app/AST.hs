module AST where

import Control.Applicative (Alternative (some))
import Data.List ()
import Numeric.Natural
import Text.Parsec
import Text.Parsec.String (Parser)
import Util

sourcePos :: Monad m => ParsecT s u m SourcePos
sourcePos = statePos `fmap` getParserState

data Node = Parens [Node] SourcePos SourcePos | Token String SourcePos SourcePos deriving (Show)

parens :: Parser Node
parens = do
  from <- sourcePos
  parens' <- between
          (oneOf "({[")
          (oneOf ")}]")
          (spaces *> (Parens <$> many node) <* spaces)
  parens' from <$> sourcePos

node :: Parser Node
node =
  do
    from <- sourcePos
    nodef <-
      Token <$> some (noneOf "(){}[] \n\t")
        <|> between
          (oneOf "({[")
          (oneOf "])}")
          (spaces *> (Parens <$> many node) <* spaces)
    to <- sourcePos
    spaces
    pure $ nodef from to

file :: Parser Node
file =
  do
    from <- sourcePos
    nodes <- many (spaces *> parens <* spaces)
    to <- sourcePos
    spaces
    eof
    pure $ Parens nodes from to

parseParens :: String -> Node
parseParens s = case parse file "" s of
  Right result -> result
  Left failure -> error (show failure)

parseParensE :: String -> Either ParseError Node
parseParensE = parse file ""

parensRanges :: Node -> [(SourcePos, SourcePos)]
parensRanges (Parens children from to) = (from, to) : concatMap parensRanges children
parensRanges (Token _ from to) = [(from, to)]

traverseAST :: [Natural] -> Node -> Either Node Node
traverseAST (i : is) node'@(Parens children _ _) = case children ?+! i of
  Just node'' -> traverseAST is node''
  Nothing -> Left node'
traverseAST (_ : _) node' = Left node'
traverseAST [] n = Right n

fromToToPosLen :: String -> SourcePos -> Natural
fromToToPosLen s from = fromIntegral $ lpos + fromIntegral (sourceColumn from) - 1
  where
    (lpos, _) = splitLines s !! (sourceLine from - 1)

fromToToPosLens :: String -> (SourcePos, SourcePos) -> (Natural, Natural)
fromToToPosLens s (from, to) = (c, c' - c)
  where
    c = fromToToPosLen s from
    c' = fromToToPosLen s to
