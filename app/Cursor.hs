{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Cursor where

import AST
  ( Node (Parens, Token),
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

nextTypeSibling :: String -> Maybe Node -> Cursor -> Cursor
nextTypeSibling s ast cs@(c@(CN t _):cs') = coerceCursor s ast cs (lastDef c (filter ((t <) . cursorType) (childrenOf s ast cs')) : cs')
nextTypeSibling _ _ [] = []

prevTypeSibling :: String -> Maybe Node -> Cursor -> Cursor
prevTypeSibling s ast cs@(c@(CN t _):cs') = coerceCursor s ast cs (headDef c (filter ((t >) . cursorType) (childrenOf s ast cs')) : cs')
prevTypeSibling _ _ [] = []

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
  Maybe Node ->
  CursorType ->
  MetaCursor ->
  (String -> [(Natural, Natural)]) ->
  [([CursorNode], (Natural, Natural))]
ranger s ast t rest splitter = concat [[(CN t i : cur, (pos' + pos, len')) | (i, (pos', len')) <- zip [0 ..] (splitter (slice (pos, len) s) ++ [(len, 0) | len /= 0])] | (cur, (pos, len)) <- reachableRanges s ast rest]

reachableRanges :: String -> Maybe Node -> MetaCursor -> [(Cursor, (Natural, Natural))]
reachableRanges s _ [] = [([], (0, genericLength s))]
reachableRanges s ast (Char : rest) = concat [[(CN Char c : cur, (pos + c, i)) | (c, i) <- zip (natRange 0 len) (repeat 1) ++ [(len, 0)]] | (cur, (pos, len)) <- reachableRanges s ast rest]
reachableRanges s ast (Word : rest) = ranger s ast Word rest splitWords
reachableRanges s ast (WORD : rest) = ranger s ast WORD rest splitWORDs
reachableRanges s ast (Sentence : rest) = ranger s ast Sentence rest splitSentences
reachableRanges s ast (Line : rest) = ranger s ast Line rest splitLines
reachableRanges s ast (Paragraph : rest) = ranger s ast Paragraph rest splitParas
reachableRanges s ast path@(ASTNode : _) =
  let ns = takeWhile (== ASTNode) path
   in concat
        [ let s' = slice (pos, len) s
           in case ast of
                Just (Parens result _ _) -> ranges ns result
                  where
                    ranges (ASTNode : rc) nodes =
                      concat
                        [ case node of
                            Parens nodes' pos' len' -> ([(CN ASTNode i : cur, (pos + pos', len')) | null rc]) ++ map (_1 %~ (++ [CN ASTNode i])) (ranges rc nodes')
                            Token pos' len' | null rc -> [(CN ASTNode i : cur, (pos + pos', len'))]
                            _ -> []
                          | (i, node) <- zip [0 ..] nodes
                        ]
                    ranges _ _ = []
                _ -> [([], (0, genericLength s))]
          | (cur, (pos, len)) <- reachableRanges s ast (dropWhile (== ASTNode) path)
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

findCursor :: String -> Maybe Node -> Cursor -> MetaCursor -> Cursor
findCursor _ _ _ [] = []
findCursor s ast c mc@(_:mc') = case cursors of
  x : _ -> fst x
  _ -> findCursor s ast c mc'
  where
    cmp = metaCursor c `compareCursors` mc
    ranges = reachableRanges s ast mc
    r@(pos, len) = getCursorRange s ast c

    covered = filter (\(_, r') -> r `covers` r') ranges
    cover = filter (\(_, r') -> r' `covers` r) ranges

    before = reverse $ filter (\(_, (pos', len')) -> pos' < pos && pos' + len' >= pos && pos' + len' <= pos) ranges
    after = filter (\(_, (pos', _)) -> pos' > pos && pos' < pos + len) ranges

    cursors = covered ++ cover ++ (if cmp == LT then before ++ after else after ++ before)

findCursor' :: String -> Maybe Node -> Cursor -> MetaCursor -> Cursor
findCursor' _ _ _ [] = []
findCursor' s ast c mc@(_:mc') = case covered ++ cover of
  x : _ -> fst x
  _ -> case findCursor' s ast c mc' of
    cur@(CN Char _ : _) -> cur
    cur -> let (pos', _) = getCursorRange s ast cur in CN Char (pos - pos') : cur
  where
    ranges = reachableRanges s ast mc
    r@(pos, _) = getCursorRange s ast c

    covered = filter (\(_, r') -> r `covers` r') ranges
    cover = filter (\(_, r') -> r' `covers` r) ranges


metaCursor :: Cursor -> MetaCursor
metaCursor = map cursorType

coerceCursor :: String -> Maybe Node -> Cursor -> Cursor -> Cursor
coerceCursor s ast c c' = findCursor s ast c (metaCursor c')

cursorRanger :: String -> Maybe Node -> (String -> Natural -> (Natural, Natural)) -> Natural -> Cursor -> (Natural, Natural)
cursorRanger buf ast splitter i xs = let (pos, len) = getCursorRange buf ast xs in _1 +~ pos $ splitter (slice (pos, len) buf) i

getCursorRange :: String -> Maybe Node -> Cursor -> (Natural, Natural)
getCursorRange buf _ [] = (0, genericLength buf)
getCursorRange buf ast  ((CN Char c) : xs) = let (pos, len) = getCursorRange buf ast xs in (pos + min len c, min 1 $ len ?- c)
getCursorRange buf ast ((CN Word w) : xs) = cursorRanger buf ast nthWord w xs
getCursorRange buf ast ((CN WORD w) : xs) = cursorRanger buf ast nthWORD w xs
getCursorRange buf ast ((CN Sentence s) : xs) = cursorRanger buf ast nthSentence s xs
getCursorRange buf ast ((CN Line l) : xs) = cursorRanger buf ast nthLine l xs
getCursorRange buf ast ((CN Paragraph p) : xs) = cursorRanger buf ast nthPara p xs
getCursorRange buf ast cur@((CN ASTNode _) : _) =
  let (pos_, len_) = getCursorRange buf ast (dropWhile ((ASTNode ==) . cursorType) cur)
   in case ast of
        Just result -> _1 +~ pos_ $ case traverseAST (reverse $ map idx (takeWhile ((ASTNode ==) . cursorType) cur)) result of
          Right (Token pos pos') -> (pos, pos')
          Right (Parens _ pos pos') -> (pos, pos')
          Left (Token _ pos') -> (pos', pos')
          Left (Parens _ _ pos') -> (pos', pos')
        Nothing -> (0, 0)

getCursorToNextSiblingRange :: String -> Maybe Node -> Cursor -> (Natural, Natural)
getCursorToNextSiblingRange s ast cur = (pos, pos' - pos)
  where
    (pos, _) = getCursorRange s ast cur
    (pos', _) = getCursorRange s ast $ nextSibling s ast cur

getCharacterPos :: String -> Maybe Node -> Cursor -> Natural
getCharacterPos s ast = fst . getCursorRange s ast

getCursorRanges :: String -> Maybe Node -> Cursors -> NonEmpty (Natural, Natural)
getCursorRanges buf ast = fmap (getCursorRange buf ast)

getCursorToNextSiblingRanges :: String -> Maybe Node -> Cursors -> [(Natural, Natural)]
getCursorToNextSiblingRanges s ast = NE.toList . fmap (getCursorToNextSiblingRange s ast)

characterPosition :: String -> Natural -> (Natural, Natural)
characterPosition buf i =
  let linesBefore = splitLines (genericTake i buf)
      lineNo = genericLength linesBefore ?- 1
      characterNo = case linesBefore of
        (_ : _) -> snd $ last linesBefore
        _ -> 0
   in (lineNo, characterNo)

getCursorAbsolute :: String -> Maybe Node -> Cursor -> ((Natural, Natural), (Natural, Natural))
getCursorAbsolute buf ast cursor =
  let (pos, len) = getCursorRange buf ast cursor
   in (characterPosition buf pos, characterPosition buf (pos + len))

canHaveChildrenOfType :: Cursor -> CursorType -> Bool
canHaveChildrenOfType [] _ = True
canHaveChildrenOfType (CN t' _ : _) t = t' `consistsOf` t

validate :: Cursor -> Bool
validate = foldlPairs (\prev (CN a _, CN b _) -> prev && b `consistsOf` a) True

updateCursor :: Cursor -> Cursor -> Cursor
updateCursor old new = if validate new then new else old

childrenOfType :: String -> Maybe Node -> Cursor -> CursorType -> [CursorNode]
childrenOfType s ast cur t =
  if not $ cur `canHaveChildrenOfType` t
    then []
    else map (CN t) $ case t of
      Char -> [0 .. (snd $ getCursorRange s ast cur)]
      Word -> itemsIn s (getCursorRange s ast cur) splitWords
      WORD -> itemsIn s (getCursorRange s ast cur) splitWORDs
      Line -> itemsIn s (getCursorRange s ast cur) splitLines
      Sentence -> itemsIn s (getCursorRange s ast cur) splitSentences
      Paragraph -> itemsIn s (getCursorRange s ast cur) splitParas
      ASTNode -> case ast of
        Just root@(Parens {}) -> case traverseAST (reverse $ map idx cur) root of
          Right (Parens children' _ _) -> [0 .. genericLength children']
          _ -> []
        _ -> []

childrenOf :: String -> Maybe Node -> Cursor -> [CursorNode]
childrenOf s ast c = concatMap (childrenOfType s ast c) $ reverse [minBound .. maxBound]

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

firstChild :: String -> Maybe Node -> Cursor -> [CursorNode]
firstChild s ast cursor = case childrenOf s ast cursor of
  c : _ -> c : cursor
  _ -> cursor

bestASTNode :: String -> Maybe Node -> Cursor -> Cursor
bestASTNode s ast cur = case concatMap (go []) ast of [] -> []; c : _ -> c
  where
    range = getCursorRange s ast cur

    astNodeCovers (Parens _ pos' len') = (pos', len') `covers` range
    astNodeCovers (Token pos' len') = (pos', len') `covers` range

    go :: Cursor -> Node -> [Cursor]
    go c (Token {}) = [c]
    go c self@(Parens nodes _ _)
      | astNodeCovers self = case filter (astNodeCovers . snd) (zip [0 ..] nodes) of
          [] -> [c]
          nodes' -> concatMap (\(i, n) -> go (CN ASTNode i : c) n) nodes'
      | otherwise = []

-- moveTo :: String -> Cursor -> Cursor -> Cursor

nextSibling :: String -> Maybe Node -> Cursor -> Cursor
nextSibling s ast (CN t i : rest) | i + 1 < genericLength (childrenOfType s ast rest t) = CN t (i + 1) : rest
nextSibling s ast (c : rest) = firstSibling s ast $ c : nextSibling s ast rest
nextSibling _ _ [] = []

firstSibling :: String -> Maybe Node -> Cursor -> Cursor
firstSibling s ast (CN t _ : rest) = case childrenOfType s ast rest t of
  [] -> rest
  _ -> CN t 0 : rest
firstSibling _ _ [] = []

nextCousin :: String -> Maybe Node -> Cursor -> Cursor
nextCousin s ast (CN t i : rest) = case childrenOfType s ast ps t of
  [] -> ps -- If there are no children, go to parent
  _ -> CN t i : ps
  where ps = nextSibling s ast rest
nextCousin _ _ c = c

prevSibling :: String -> Maybe Node -> Cursor -> Cursor
prevSibling s ast (CN t i : rest) | i > 0 && i >= genericLength (childrenOfType s ast rest t) = CN t (genericLength (childrenOfType s ast rest t) ?- 2) : rest
prevSibling _ _ (CN t i : rest) | i > 0 = CN t (i - 1) : rest
prevSibling s ast (c : rest) = lastSibling s ast $ c : prevSibling s ast rest
prevSibling _ _ [] = []

lastSibling :: String -> Maybe Node -> Cursor -> Cursor
lastSibling s ast (CN t _ : rest) = case childrenOfType s ast rest t of
  [] -> rest
  c -> CN t (genericLength c ?- 1) : rest
lastSibling _ _ [] = []

prevCousin :: String -> Maybe Node -> Cursor -> Cursor
prevCousin s ast (CN t i : rest) = case childrenOfType s ast ps t of
  [] -> ps -- If there are no children, go to parent
  _ -> CN t i : ps
  where ps = prevSibling s ast rest
prevCousin _ _ c = c

nextChar :: String -> Maybe Node -> Cursor -> Cursor
nextChar s ast c = findCursor' s ast [CN Char (getCharacterPos s ast c + 1)] (metaCursor c)

prevChar :: String -> Maybe Node -> Cursor -> Cursor
prevChar s ast c = findCursor' s ast [CN Char (getCharacterPos s ast c ?- 1)] (metaCursor c)

lastChar :: String -> Maybe Node -> Cursor -> Cursor
lastChar s ast (_:c) = CN Char (pos' - pos) : c
  where
    (pos, _) = getCursorRange s ast c
    (pos', _) = getCursorRange s ast $ nextSibling s ast c
lastChar _ _ _ = undefined

parent :: Cursor -> Cursor
parent = drop 1

type Cursor = [CursorNode]

type MetaCursor = [CursorType]

type Cursors = NonEmpty Cursor