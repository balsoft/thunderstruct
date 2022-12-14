{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Cursor where

import AST
  ( Node (Parens, Token),
    fromToToPosLens,
    parseParensE,
    traverseAST,
  )
import Control.Lens
import Data.List (genericLength, genericTake)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Numeric.Natural (Natural)
import Safe (toEnumSafe, headDef, lastDef)
import Util
import Prelude hiding (Char, Word)
import qualified Prelude

data CursorType = Char | Word | WORD | Sentence | Line | Paragraph | ASTNode deriving (Show, Eq, Ord, Enum, Bounded)

cursorByChar :: Prelude.Char -> Maybe CursorType
cursorByChar 'c' = Just Char
cursorByChar 'w' = Just Word
cursorByChar 'W' = Just WORD
cursorByChar 's' = Just Sentence
cursorByChar 'l' = Just Line
cursorByChar 'p' = Just Paragraph
cursorByChar 'a' = Just ASTNode
cursorByChar _ = Nothing


data CursorNode = CN {cursorType :: CursorType, idx :: Natural} deriving (Eq)

$(makeLensesFor [("cursorType", "_cursorType"), ("idx", "_idx")] ''CursorNode)

instance Show CursorNode where
  show (CN t i) = show t ++ " " ++ show i

consistsOf :: CursorType -> CursorType -> Bool
ASTNode `consistsOf` ASTNode = True
ASTNode `consistsOf` Word = True
ASTNode `consistsOf` WORD = True
ASTNode `consistsOf` Char = True
Paragraph `consistsOf` Line = True
Paragraph `consistsOf` Sentence = True
Paragraph `consistsOf` Word = True
Paragraph `consistsOf` WORD = True
Paragraph `consistsOf` Char = True
Line `consistsOf` Sentence = True
Line `consistsOf` Word = True
Line `consistsOf` WORD = True
Line `consistsOf` Char = True
Sentence `consistsOf` Word = True
Sentence `consistsOf` WORD = True
Sentence `consistsOf` Char = True
Word `consistsOf` Char = True
_ `consistsOf` _ = False

prevConstituent :: CursorType -> CursorType
prevConstituent t = case filter (t `consistsOf`) [minBound .. t] of [] -> t; x -> last x

prevConstituent' :: CursorType -> CursorType
prevConstituent' t = case filter (\x -> t `consistsOf` x && t /= x) [minBound .. t] of [] -> t; x -> last x

nextConsistsOf :: CursorType -> CursorType
nextConsistsOf t = case filter (`consistsOf` t) (enumFrom t) of [] -> t; x -> head x

(>+) :: CursorNode -> Natural -> CursorNode
(CN t a) >+ b = CN t (a + b)

(>-) :: CursorNode -> Natural -> CursorNode
(CN t a) >- b = CN t (a ?- b)

nextTypeSibling :: String -> Cursor -> Cursor
nextTypeSibling s cs@(c@(CN t _):cs') = coerceCursor s cs (lastDef c (filter ((t <) . cursorType) (childrenOf s cs')) : cs')
nextTypeSibling _ [] = []

prevTypeSibling :: String -> Cursor -> Cursor
prevTypeSibling s cs@(c@(CN t _):cs') = coerceCursor s cs (headDef c (filter ((t >) . cursorType) (childrenOf s cs')) : cs')
prevTypeSibling _ [] = []

nthType :: Natural -> CursorType
nthType = toEnumSafe . fromIntegral

toNthType :: Natural -> CursorNode -> CursorNode
toNthType n = _cursorType .~ nthType n

toTypes :: [CursorType] -> Cursor -> Cursor
toTypes (i:is) (c:cs) = (_cursorType .~ i) c : toTypes is cs
toTypes [] cs = cs
toTypes _ [] = []

toNthIndicies :: [Natural] -> Cursor -> Cursor
toNthIndicies (i:is) (c:cs) = (_idx .~ i) c : toNthIndicies is cs
toNthIndicies [] cs = cs
toNthIndicies _ [] = []

ranger ::
  String ->
  CursorType ->
  MetaCursor ->
  (String -> [(Natural, Natural)]) ->
  [([CursorNode], (Natural, Natural))]
ranger s t rest splitter = concat [[(CN t i : cur, (pos' + pos, len')) | (i, (pos', len')) <- zip [0 ..] (splitter (slice (pos, len) s) ++ [(len, 0) | len /= 0])] | (cur, (pos, len)) <- reachableRanges s rest]

reachableRanges :: String -> MetaCursor -> [(Cursor, (Natural, Natural))]
reachableRanges s [] = [([], (0, genericLength s))]
reachableRanges s (Char : rest) = concat [[(CN Char c : cur, (pos + c, i)) | (c, i) <- zip (natRange 0 len) (repeat 1) ++ [(len, 0)]] | (cur, (pos, len)) <- reachableRanges s rest]
reachableRanges s (Word : rest) = ranger s Word rest splitWords
reachableRanges s (WORD : rest) = ranger s WORD rest splitWORDs
reachableRanges s (Sentence : rest) = ranger s Sentence rest splitSentences
reachableRanges s (Line : rest) = ranger s Line rest splitLines
reachableRanges s (Paragraph : rest) = ranger s Paragraph rest splitParas
reachableRanges s path@(ASTNode : _) =
  let ns = takeWhile (== ASTNode) path
   in concat
        [ let s' = slice (pos, len) s
           in case parseParensE (slice (pos, len) s) of
                Right (Parens result _ _) -> ranges ns result
                  where
                    ranges (ASTNode : rc) nodes =
                      concat
                        [ case node of
                            Parens nodes' from' to' -> let (pos', len') = fromToToPosLens s' (from', to') in ([(CN ASTNode i : cur, (pos + pos', len')) | null rc]) ++ map (_1 %~ (++ [CN ASTNode i])) (ranges rc nodes')
                            Token _ from' to' | null rc -> let (pos', len') = fromToToPosLens s' (from', to') in [(CN ASTNode i : cur, (pos + pos', len'))]
                            _ -> []
                          | (i, node) <- zip [0 ..] nodes
                        ]
                    ranges _ _ = []
                _ -> [([], (0, genericLength s))]
          | (cur, (pos, len)) <- reachableRanges s (dropWhile (== ASTNode) path)
        ]

-- Nice, but slow :/
-- reachableRanges s mc = go (reverse mc) []
--   where
--     go :: MetaCursor -> Cursor -> [(Cursor, (Natural, Natural))]
--     go [] [] = [([], (0, genericLength s))]
--     go [] c = [(c, getCursorRange s c)]
--     go (mc : mcs) c = concatMap (go mcs . (:c)) (childrenOfType s c mc)

compareCursors :: Ord a => [a] -> [a] -> Ordering
compareCursors (a : as) (b : bs)
  | a == b = compareCursors as bs
  | otherwise = a `compare` b
compareCursors [] [] = EQ
compareCursors [] _ = LT
compareCursors _ [] = GT

findCursor :: String -> Cursor -> MetaCursor -> Cursor
findCursor _ _ [] = []
findCursor s c mc@(_:mc') = case cursors of
  x : _ -> fst x
  _ -> findCursor s c mc'
  where
    cmp = metaCursor c `compareCursors` mc
    ranges = reachableRanges s mc
    r@(pos, len) = getCursorRange s c

    covered = filter (\(_, r') -> r `covers` r') ranges
    cover = filter (\(_, r') -> r' `covers` r) ranges

    before = reverse $ filter (\(_, (pos', len')) -> pos' < pos && pos' + len' >= pos && pos' + len' <= pos) ranges
    after = filter (\(_, (pos', _)) -> pos' > pos && pos' < pos + len) ranges

    cursors = covered ++ cover ++ (if cmp == LT then before ++ after else after ++ before)

findCursor' :: String -> Cursor -> MetaCursor -> Cursor
findCursor' _ _ [] = []
findCursor' s c mc@(_:mc') = case covered ++ cover of
  x : _ -> fst x
  _ -> case findCursor' s c mc' of
    cur@(CN Char _ : _) -> cur
    cur -> let (pos', _) = getCursorRange s cur in CN Char (pos - pos') : cur
  where
    ranges = reachableRanges s mc
    r@(pos, _) = getCursorRange s c

    covered = filter (\(_, r') -> r `covers` r') ranges
    cover = filter (\(_, r') -> r' `covers` r) ranges


metaCursor :: Cursor -> MetaCursor
metaCursor = map cursorType

coerceCursor :: String -> Cursor -> Cursor -> Cursor
coerceCursor s c c' = findCursor s c (metaCursor c')

cursorRanger :: String -> (String -> Natural -> (Natural, Natural)) -> Natural -> Cursor -> (Natural, Natural)
cursorRanger buf splitter i xs = let (pos, len) = getCursorRange buf xs in _1 +~ pos $ splitter (slice (pos, len) buf) i

getCursorRange :: String -> Cursor -> (Natural, Natural)
getCursorRange buf [] = (0, genericLength buf)
getCursorRange buf ((CN Char c) : xs) = let (pos, len) = getCursorRange buf xs in (pos + min len c, min 1 $ len ?- c)
getCursorRange buf ((CN Word w) : xs) = cursorRanger buf nthWord w xs
getCursorRange buf ((CN WORD w) : xs) = cursorRanger buf nthWORD w xs
getCursorRange buf ((CN Sentence s) : xs) = cursorRanger buf nthSentence s xs
getCursorRange buf ((CN Line l) : xs) = cursorRanger buf nthLine l xs
getCursorRange buf ((CN Paragraph p) : xs) = cursorRanger buf nthPara p xs
getCursorRange buf cur@((CN ASTNode _) : _) =
  let (pos_, len_) = getCursorRange buf (dropWhile ((ASTNode ==) . cursorType) cur)
   in case parseParensE (slice (pos_, len_) buf) of
        Right result -> _1 +~ pos_ $ fromToToPosLens buf $ case traverseAST (reverse $ map idx (takeWhile ((ASTNode ==) . cursorType) cur)) result of
          Right (Token _ pos pos') -> (pos, pos')
          Right (Parens _ pos pos') -> (pos, pos')
          Left (Token _ _ pos') -> (pos', pos')
          Left (Parens _ _ pos') -> (pos', pos')
        Left _ -> (0, 0)

getCursorToNextSiblingRange :: String -> Cursor -> (Natural, Natural)
getCursorToNextSiblingRange s cur = (pos, pos' - pos)
  where
    (pos, _) = getCursorRange s cur
    (pos', _) = getCursorRange s $ nextSibling s cur

getCharacterPos :: String -> Cursor -> Natural
getCharacterPos s = fst . getCursorRange s

getCursorRanges :: String -> Cursors -> NonEmpty (Natural, Natural)
getCursorRanges buf = fmap (getCursorRange buf)

getCursorToNextSiblingRanges :: String -> Cursors -> [(Natural, Natural)]
getCursorToNextSiblingRanges s = NE.toList . fmap (getCursorToNextSiblingRange s)

characterPosition :: String -> Natural -> (Natural, Natural)
characterPosition buf i =
  let linesBefore = splitLines (genericTake i buf)
      lineNo = genericLength linesBefore ?- 1
      characterNo = case linesBefore of
        (_ : _) -> snd $ last linesBefore
        _ -> 0
   in (lineNo, characterNo)

getCursorAbsolute :: String -> Cursor -> ((Natural, Natural), (Natural, Natural))
getCursorAbsolute buf cursor =
  let (pos, len) = getCursorRange buf cursor
   in (characterPosition buf pos, characterPosition buf (pos + len))

canHaveChildrenOfType :: Cursor -> CursorType -> Bool
canHaveChildrenOfType [] _ = True
canHaveChildrenOfType (CN t' _ : _) t = t' `consistsOf` t

validate :: Cursor -> Bool
validate = foldlPairs (\prev (CN a _, CN b _) -> prev && b `consistsOf` a) True

updateCursor :: Cursor -> Cursor -> Cursor
updateCursor old new = if validate new then new else old

childrenOfType :: String -> Cursor -> CursorType -> [CursorNode]
childrenOfType s cur t =
  if not $ cur `canHaveChildrenOfType` t
    then []
    else map (CN t) $ case t of
      Char -> [0 .. (snd $ getCursorRange s cur)]
      Word -> itemsIn s (getCursorRange s cur) splitWords
      WORD -> itemsIn s (getCursorRange s cur) splitWORDs
      Line -> itemsIn s (getCursorRange s cur) splitLines
      Sentence -> itemsIn s (getCursorRange s cur) splitSentences
      Paragraph -> itemsIn s (getCursorRange s cur) splitParas
      ASTNode -> case parseParensE s of
        Right root@(Parens {}) -> case traverseAST (reverse $ map idx cur) root of
          Right (Parens children' _ _) -> [0 .. genericLength children']
          _ -> []
        _ -> []

childrenOf :: String -> Cursor -> [CursorNode]
childrenOf s c = concatMap (childrenOfType s c) $ reverse [minBound .. maxBound]

-- childrenOfType _ (CN Char _ : _) _ = []
-- childrenOfType s cur@(CN t _ : _) Char = if t > Char then map (CN Char) [0..(snd $ getCursorRange s cur)] else []
-- childrenOfType s cur@(CN t _ : _) Word = map (CN Word) $ itemsIn s (getCursorRange s cur) words'
-- childrenOfType s cur Line = map (CN Line) $ itemsIn s (getCursorRange s cur) lines'
-- childrenOfType s cur@(CN ASTNode _ : _) ASTNode = -- childrenOfType s [] ASTNode = case parseParensE s of
--     Right (Parens children _ _) -> map (CN ASTNode) [0 .. genericLength children]
--     Right (Token _ _ _) -> [CN ASTNode 0]
--     Left _ -> []
-- childrenOfType _ _ ASTNode = []

-- childrenOf :: String -> Cursor -> [CursorNode]
-- childrenOf s c = concatMap (childrenOfType s )

-- childrenOf :: String -> Cursor -> [CursorNode]
-- childrenOf s (CN Char _ : _) = []
-- childrenOf s cur@(CN Word _ : _) = charsIn s (getCursorRange s cur)
-- childrenOf s cur@(CN Line _ : _) = (wordsIn s <> charsIn s) (getCursorRange s cur)
-- childrenOf s cur@(CN ASTNode _ : _) = () <> (wordsIn s <> charsIn s) (getCursorRange s cur)
-- childrenOf s [] =

--     <> map (CN Line) [0 .. genericLength (lines' s)]
--     <> (charsIn s <> wordsIn s) (0, genericLength s)

nextline :: Cursor -> Cursor
nextline (CN Line l : rest) = CN Line (l + 1) : nextline rest
nextline (c : rest) = c : nextline rest
nextline [] = []

firstChild :: String -> Cursor -> [CursorNode]
firstChild s cursor = case childrenOf s cursor of
  c : _ -> c : cursor
  _ -> cursor

bestASTNode :: String -> Cursor -> Cursor
bestASTNode s cur = case concatMap (go []) allNodes of [] -> []; c : _ -> c
  where
    range = getCursorRange s cur

    allNodes = case parseParensE s of
      Right n -> Just n
      Left _ -> Nothing

    astNodeCovers (Parens _ from' to') = fromToToPosLens s (from', to') `covers` range
    astNodeCovers (Token _ from' to') = fromToToPosLens s (from', to') `covers` range

    go :: Cursor -> Node -> [Cursor]
    go c (Token {}) = [c]
    go c self@(Parens nodes _ _)
      | astNodeCovers self = case filter (astNodeCovers . snd) (zip [0 ..] nodes) of
          [] -> [c]
          nodes' -> concatMap (\(i, n) -> go (CN ASTNode i : c) n) nodes'
      | otherwise = []

-- moveTo :: String -> Cursor -> Cursor -> Cursor

nextSibling :: String -> Cursor -> Cursor
nextSibling s (CN t i : rest) | i + 1 < genericLength (childrenOfType s rest t) = CN t (i + 1) : rest
nextSibling s (c : rest) = firstSibling s $ c : nextSibling s rest
nextSibling _ [] = []

firstSibling :: String -> Cursor -> Cursor
firstSibling s (CN t _ : rest) = case childrenOfType s rest t of
  [] -> rest
  _ -> CN t 0 : rest
firstSibling _ [] = []

nextCousin :: String -> Cursor -> Cursor
nextCousin s (CN t i : rest) = case childrenOfType s ps t of
  [] -> ps -- If there are no children, go to parent
  _ -> CN t i : ps
  where ps = nextSibling s rest
nextCousin _ c = c

prevSibling :: String -> Cursor -> Cursor
prevSibling s (CN t i : rest) | i > 0 && i >= genericLength (childrenOfType s rest t) = CN t (genericLength (childrenOfType s rest t) ?- 2) : rest
prevSibling _ (CN t i : rest) | i > 0 = CN t (i - 1) : rest
prevSibling s (c : rest) = lastSibling s $ c : prevSibling s rest
prevSibling _ [] = []

lastSibling :: String -> Cursor -> Cursor
lastSibling s (CN t _ : rest) = case childrenOfType s rest t of
  [] -> rest
  c -> CN t (genericLength c ?- 1) : rest
lastSibling _ [] = []

prevCousin :: String -> Cursor -> Cursor
prevCousin s (CN t i : rest) = case childrenOfType s ps t of
  [] -> ps -- If there are no children, go to parent
  _ -> CN t i : ps
  where ps = prevSibling s rest
prevCousin _ c = c

nextChar :: String -> Cursor -> Cursor
nextChar s c = findCursor' s [CN Char (getCharacterPos s c + 1)] (metaCursor c)

prevChar :: String -> Cursor -> Cursor
prevChar s c = findCursor' s [CN Char (getCharacterPos s c ?- 1)] (metaCursor c)

lastChar :: String -> Cursor -> Cursor
lastChar s (_:c) = CN Char (pos' - pos) : c
  where
    (pos, _) = getCursorRange s c
    (pos', _) = getCursorRange s $ nextSibling s c
lastChar _ _ = undefined

parent :: Cursor -> Cursor
parent = drop 1

type Cursor = [CursorNode]

type MetaCursor = [CursorType]

type Cursors = NonEmpty Cursor