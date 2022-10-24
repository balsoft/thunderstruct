module Util where

import Numeric.Natural
import Data.List (genericTake, genericLength, genericDrop)

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
deleteAt idx xs = lft ++ rgt
  where (lft, _:rgt) = splitAt (fromIntegral idx) xs

deleteMany :: Integral idx => (idx, idx) -> [a] -> [a]
deleteMany (idx, cnt) xs = genericTake idx xs ++ genericDrop (idx + cnt) xs

splitBy' :: (Char -> Bool) -> [Char] -> [[Char]]
splitBy' _ "" = [""]
splitBy' predicate s =
  cons
    ( case break predicate s of
        (l, s') ->
          ( l <> case s' of
              [] -> ""
              x : _ -> [x],
            case s' of
              [] -> []
              _ : s'' -> splitBy' predicate s''
          )
    )
  where
    cons ~(h, t) = h : t

lines' :: [Char] -> [[Char]]
lines' = splitBy' (=='\n')

words' :: [Char] -> [[Char]]
words' = splitBy' (`elem` " \n\t")


slice :: (Natural, Natural) -> [a] -> [a]
slice (pos, len) xs = genericTake len $ genericDrop pos xs

(!+!) :: Integral idx => [a] -> idx -> a
lst !+! n = lst !! fromIntegral n

(?+!) :: Integral idx => [a] -> idx -> Maybe a
lst ?+! n = if n < genericLength lst then Just (lst !+! n) else Nothing

(?-) :: (Integral a1, Integral a2, Integral a3) => a1 -> a2 -> a3
a ?- b = fromIntegral $ max 0 (fromIntegral a - fromIntegral b :: Int)

nthThing :: Integral idx => ([a] -> [[a]]) -> [a] -> idx -> (idx, idx)
nthThing splitter buf n =
  let
    thingsBefore = genericTake n $ splitter buf
    pos = genericLength $ concat thingsBefore
    split = splitter buf
    len = if n < genericLength split then genericLength (split !+! n) else 0
  in (pos, len)

natRange :: Natural -> Natural -> [Natural]
natRange f t = fromIntegral <$> [fromIntegral f .. (fromIntegral t - 1 :: Int)]

rangeWith :: (Foldable t1, Num b) => t2 -> (t2 -> t1 [a]) -> [(b, b)]
rangeWith s f = reverse $ foldl (\lst word -> case lst of ((pos, len) : _) -> (pos + len, genericLength word) : lst; [] -> [(0, genericLength word)]) [] (f s)

covers :: (Ord a, Num a) => (a, a) -> (a, a) -> Bool
covers (p, l) (p', l') = p >= p' && p + l <= p' + l'

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
