{-# LANGUAGE GADTs #-}

module TypedCursor where

import Prelude hiding (Char, Word)
import Data.Functor.Identity

data Char
data Word
-- data WORD
-- data Sentence
data Line
-- data Paragraph
data ASTNode

data ConsistsOf a b where
  WordOfChar :: ConsistsOf Word Char
  LineOfChar :: ConsistsOf Line Char
  LineOfWord :: ConsistsOf Line Word
  ASTNodeOfChar :: ConsistsOf ASTNode Char
  ASTNodeOfWord :: ConsistsOf ASTNode Word
  ASTNodeOfASTNode :: ConsistsOf ASTNode ASTNode


data CursorTree f a t where
  CursorTreeNode :: ConsistsOf t t' -> a -> (f (CursorTree f a t')) -> CursorTree f a t
  CursorTreeLeaf :: a -> CursorTree f a t
  deriving (Show)

example1 = CursorTreeNode WordOfChar () (CursorTreeLeaf ())

example2 = CursorTreeNode LineOfWord () (Identity (CursorTreeNode WordOfChar () (Identity (CursorTreeLeaf ()))))