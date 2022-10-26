{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Util where

import Numeric.Natural
import Data.List (genericTake, genericLength, genericDrop, isPrefixOf)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List (sort)
import Data.Default.Class
import Control.Lens

remove :: Eq a => a -> [a] -> [a]
remove element = filter (/= element)

insertAt :: Integral idx => idx -> a -> [a] -> [a]
insertAt _ a [] = [a]
insertAt pos a (x : xs)
  | pos == 0 = a : x : xs
  | pos > 0 = x : insertAt (pos - 1) a xs
  | otherwise = x : insertAt (pos + fromIntegral (length (x : xs))) a xs

insertMany :: Integral idx => idx -> [a] -> [a] -> [a]
insertMany idx elems lst = genericTake idx lst ++ elems ++ genericDrop idx lst

deleteAt :: Integral idx => idx -> [a] -> [a]
deleteAt idx xs = lft ++ drop 1 rgt
  where
    (lft, rgt) = splitAt (fromIntegral idx) xs

deleteMany :: Integral idx => (idx, idx) -> [a] -> [a]
deleteMany (idx, cnt) xs = genericTake idx xs ++ genericDrop (idx + cnt) xs

deleteMultiple :: (Integral idx, Ord idx) => [(idx, idx)] -> [a] -> [a]
deleteMultiple ranges buf = go (sort ranges) buf 0
  where
    go rs@((pos', len'):rs') s pos
      | pos' == pos = go rs' (genericDrop len' s) (pos + len')
      | pos' < pos && pos' + len' >= pos = go rs' (genericDrop (len' ?- (pos ?- 1)) s) (pos' + len')
      | pos' < pos = go rs' s pos
      | otherwise = genericTake (pos' - pos) s ++ go rs (genericDrop (pos' ?- pos) s) pos'
    go [] s _ = s

-- splitBy' :: (a -> Bool) -> [a] -> [(Natural, Natural)]
-- splitBy' predicate s = go s 0
--   where
--     cons ~(h, t) = h : t

--     go s' pos = cons
--       ( case break predicate s' of
--           (l, s'') ->
--             ( pos,
--             --  + case s' of
--             --     [] -> ""
--             --     x : _ -> [x],
--               case s' of
--                 [] -> []
--                 _ : s'' -> go 
--             )
--       )

-- lines' :: [Char] -> [[Char]]
-- lines' = splitBy' (=='\n')

-- words' :: [Char] -> [[Char]]
-- words' = splitBy' (`elem` " \n\t")


slice :: (Natural, Natural) -> [a] -> [a]
slice (pos, len) xs = genericTake len $ genericDrop pos xs

(!+!) :: Integral idx => [a] -> idx -> a
lst !+! n = lst !! fromIntegral n

(?+!) :: Integral idx => [a] -> idx -> Maybe a
lst ?+! n = if n < genericLength lst then Just (lst !+! n) else Nothing

(?-) :: (Integral a1, Integral a2, Integral a3) => a1 -> a2 -> a3
a ?- b = fromIntegral $ max 0 (fromIntegral a - fromIntegral b :: Int)

splitBy' :: (a -> Bool) -> [a] -> [(Natural, Natural)]
-- splitBy' predicate str = go predicate str 0
--   where
--     go _ [] _ = []
--     go p s pos = let (l, s') = break predicate s; l' = genericLength l in (pos, l') : go p (case s' of [s''] | predicate s'' -> s'; _:rest -> rest; [] -> []) (pos + l' + 1)
splitBy' _ [] = []
splitBy' predicate str = go str 0 0
  where
    go [] pos len = [(pos - len, len)]
    go (x:xs) pos len
      | predicate x = (pos - len, len) : go xs (pos + 1) 0
      | otherwise = go xs (pos + 1) (len + 1)

splitBy'' :: ([a] -> Natural) -> [a] -> [(Natural, Natural)]
splitBy'' _ [] = []
splitBy'' predicate str = go str 0 0
  where
    go [] pos len = [(pos - len, len)]
    go xs pos len = case predicate xs of
      0 -> go (drop 1 xs) (pos + 1) (len + 1)
      len' -> (pos - len, len) : go (genericDrop len' xs) (pos + len') 0

if' :: Bool -> Natural
if' True = 1
if' False = 0

wordSep :: String -> Natural
wordSep = if' . (`elem` " \n\t") . head
lineSep :: String -> Natural
lineSep = if' . (== '\n') . head
paraSep :: String -> Natural
paraSep = (* 2) . if' . isPrefixOf "\n\n"
sentSep :: [Char] -> Natural
sentSep = if' . (== '.') . head

splitWords :: [Char] -> [(Natural, Natural)]
splitWords = splitBy'' wordSep
splitLines :: [Char] -> [(Natural, Natural)]
splitLines = splitBy'' lineSep
splitParas :: [Char] -> [(Natural, Natural)]
splitParas = splitBy'' paraSep
splitSentences :: [Char] -> [(Natural, Natural)]
splitSentences = splitBy'' sentSep

nthThing :: ([a] -> Natural) -> [a] -> Natural -> (Natural, Natural)
nthThing predicate buf n = case s ?+! n of
  Just s' -> s'
  Nothing -> case s of
    _:_ -> (uncurry (+) (last s), 0)
    _ -> (0, 0)
  where s = splitBy'' predicate buf

nthWord :: [Char] -> Natural -> (Natural, Natural)
nthWord = nthThing wordSep
nthLine :: [Char] -> Natural -> (Natural, Natural)
nthLine = nthThing lineSep
nthPara :: [Char] -> Natural -> (Natural, Natural)
nthPara = nthThing paraSep
nthSentence :: [Char] -> Natural -> (Natural, Natural)
nthSentence = nthThing sentSep

natRange :: Natural -> Natural -> [Natural]
natRange f t = fromIntegral <$> [fromIntegral f .. (fromIntegral t - 1 :: Int)]

-- rangeWith s f = reverse $ foldl (\lst word -> case lst of ((pos, len) : _) -> (pos + len, genericLength word) : lst; [] -> [(0, genericLength word)]) [] (f s)

covers :: (Ord a, Num a) => (a, a) -> (a, a) -> Bool
covers (p, l) (p', l') = p <= p' && p + l >= p' + l'

natOr0 :: Int -> Natural
natOr0 x = fromIntegral (max 0 x)

init' :: [a] -> [a]
init' [] = []
init' x = init x

foldlPairs :: Foldable t => (b -> (a, a) -> b) -> b -> t a -> b
foldlPairs f i c = snd $ foldl step (Nothing, i) c
  where
    step (Just prev, res) cur = (Just cur, f res (prev, cur))
    step (Nothing, res) cur = (Just cur, res)

itemsIn :: (Integral i1, Integral i2, Num a1, Enum a1) => [a2] -> (i2, i1) -> ([a2] -> [a3]) -> [a1]
itemsIn s (pos, len) splitter = [0 .. genericLength (splitter $ genericTake len $ genericDrop pos s)]

neTailSafe :: NonEmpty a -> NonEmpty a
neTailSafe cur = case cur of (_ :| (cur':rest)) -> cur' :| rest; _ -> cur

data History a = History { past :: [a], future :: [a] } deriving (Show, Eq)

instance Default (History a) where
  def = History [] []

$(makeLensesFor [("past", "_past"), ("future", "_future")] ''History)

backToTheFuture :: History a -> History a
backToTheFuture (History past (entry:rest)) = History (entry : past) rest
backToTheFuture a = a

turnBackTime :: History a -> History a
turnBackTime (History (entry:rest) future) = History rest (entry : future)
turnBackTime a = a

currentHistItem :: History a -> Maybe a
currentHistItem (History (entry:_) _) = Just entry
currentHistItem _ = Nothing

nextHistItem :: History a -> Maybe a
nextHistItem (History _ (entry:_)) = Just entry
nextHistItem _ = Nothing

newItem :: Eq a => a -> History a -> History a
newItem a h@(History {..}) = case currentHistItem h of
  Just a' | a' == a -> h
  _ -> History (a : past) []

maybeSetter :: (b -> a -> a) -> ASetter s t a a -> Maybe b -> s -> t
maybeSetter g f x = runIdentity . f (Identity . maybe id g x)

(.?) :: ASetter s t a a -> Maybe a -> s -> t
(.?) = maybeSetter const

(%?) :: ASetter s t a a -> Maybe (a -> a) -> s -> t
(%?) = maybeSetter id