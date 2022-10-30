{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE NamedFieldPuns #-}

module Cursor where

import Prelude hiding (Char, Word)
import Data.Functor.Identity

import Data.Foldable (toList)
import Text.Parsec hiding (Line)
import Text.Parsec.String (Parser)
import Control.Monad
import Control.Applicative (some)
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Numeric.Natural (Natural)
import Util
import Data.Maybe (maybeToList)


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

example1 :: CursorTree [] Word ()
example1 = CursorTreeNode Word () [CursorTreeLeaf Char ()]

example2 :: CursorTree Identity Line ()
example2 = CursorTreeNode Line () (Identity (CursorTreeNode Word () (Identity (CursorTreeLeaf Char ()))))

-- class Functor s => Selector s where
--   select :: s [a] -> [a]

-- instance Selector Identity where
--   select (Identity a) = a

-- instance Selector Maybe where
--   select (Just a) = a
--   select Nothing = []

-- data Nth a = Nth Natural a deriving (Functor, Foldable, Traversable)

-- instance Selector Nth where
--   select (Nth n a) = maybeToList (a ?+! n)

-- data Many a = Many [Natural] a deriving (Functor, Foldable, Traversable)

-- instance Selector Many where
--   select (Many n a) = fmap snd $ filter ((`elem` n) . fst) $ zip [0..] a

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

