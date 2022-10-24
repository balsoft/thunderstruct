{-# LANGUAGE TypeFamilies #-}

module Cursor where

import AST
    ( fromToToPosLens, parseParensE, traverseAST, Node(Parens, Token) )
import Control.Lens ( (%~), (+~), Field1(_1) )
import Data.List (genericLength, genericTake)
import Data.List.NonEmpty (NonEmpty)
import Numeric.Natural (Natural)
import Util
import Prelude hiding (Char, Word)
import Debug.Trace (traceShowId)

data CursorType = Char | Word | Line | ASTNode deriving (Show, Eq, Ord, Enum, Bounded)

data CursorNode = CN {cursorType :: CursorType, idx :: Natural} deriving (Eq)

instance Show CursorNode where
  show (CN t i) = show t ++ " " ++ show i

consistsOf :: CursorType -> CursorType -> Bool
ASTNode `consistsOf` ASTNode = True
ASTNode `consistsOf` Line = True
ASTNode `consistsOf` Word = True
ASTNode `consistsOf` Char = True
Line `consistsOf` Char = True
Line `consistsOf` Word = True
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

nextType :: CursorNode -> CursorNode
nextType (CN t a) = CN (prevConstituent' t) a

prevType :: CursorNode -> CursorNode
prevType (CN t a) = CN (nextConsistsOf t) a

reachableRanges :: String -> MetaCursor -> [(Cursor, (Natural, Natural))]
reachableRanges s [] = [([], (0, genericLength s))]
reachableRanges s (Char : rest) = let r = reachableRanges s rest in concat [[(CN Char c : cur, (pos + c, 1)) | c <- natRange 0 len] | (cur, (pos, len)) <- r] ++ let (cur, (pos, len)) = last r in [(CN Char (pos + len) : cur, (pos + len, 0))]
reachableRanges s (Word : rest) = concat [[(CN Word i : cur, (pos' + pos, len')) | (i, (pos', len')) <- zip [0 ..] (splitWords (slice (pos, len) s))] | (cur, (pos, len)) <- reachableRanges s rest]
reachableRanges s (Line : rest) = concat [[(CN Line i : cur, (pos' + pos, len')) | (i, (pos', len')) <- zip [0 ..] (splitLines (slice (pos, len) s))] | (cur, (pos, len)) <- reachableRanges s rest]
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
                            Parens nodes' from to -> let (pos', len') = fromToToPosLens s' (from, to) in ([(CN ASTNode i : cur, (pos + pos', len')) | null rc]) ++ map (_1 %~ (++ [CN ASTNode i])) (ranges rc nodes')
                            Token _ from to | null rc -> let (pos', len') = fromToToPosLens s' (from, to) in [(CN ASTNode i : cur, (pos + pos', len'))]
                            _ -> []
                          | (i, node) <- zip [0 ..] nodes
                        ]
                    ranges _ _ = []
                _ -> []
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
findCursor s c mc = case (ideal, case cmp of LT -> after; _ -> before) of
  (cur : _, _) -> fst cur
  ([], []) -> []
  ([], curs) -> fst $ last curs
  where
    cmp = metaCursor c `compareCursors` mc
    ranges = reachableRanges s mc
    r = getCursorRange s c

    covered = filter (\(_, r') -> r `covers` r') ranges
    cover = filter (\(_, r') -> r' `covers` r) ranges
    ideal = cover ++ covered

    before = filter (\(_, r') -> r' < r) ranges
    after = filter (\(_, r') -> r' > r) ranges

metaCursor :: Cursor -> MetaCursor
metaCursor = map cursorType

coerceCursor :: String -> Cursor -> Cursor -> Cursor
coerceCursor s c c' = findCursor s c (metaCursor c')

getCursorRange :: String -> Cursor -> (Natural, Natural)
getCursorRange buf [] = (0, genericLength buf)
getCursorRange buf ((CN Char c) : xs) = let (pos, len) = getCursorRange buf xs in (pos + min len c, min 1 $ len ?- c)
getCursorRange buf ((CN Word w) : xs) = let (pos, len) = getCursorRange buf xs in _1 +~ pos $ nthWord (slice (pos, len) buf) w
getCursorRange buf ((CN Line l) : xs) = let (pos, len) = getCursorRange buf xs in _1 +~ pos $ nthLine (slice (pos, len) buf) l
getCursorRange buf xs
  | all ((ASTNode ==) . cursorType) xs = case parseParensE buf of
      Right result -> fromToToPosLens buf $ case traverseAST (reverse $ map idx xs) result of
        Right (Token _ pos pos') -> (pos, pos')
        Right (Parens _ pos pos') -> (pos, pos')
        Left (Token _ _ pos') -> (pos', pos')
        Left (Parens _ _ pos') -> (pos', pos')
      Left _ -> (0, 0)
getCursorRange _ _ = (0, 0)

getCharacterPos :: String -> Cursor -> Natural
getCharacterPos s = fst . getCursorRange s

-- getCursorRange _ _ = error "Not implemented"

getCursorRanges :: String -> Cursors -> NonEmpty (Natural, Natural)
getCursorRanges buf = fmap (getCursorRange buf)

characterPosition :: String -> Natural -> (Natural, Natural)
characterPosition buf i =
  let linesBefore = splitLines (genericTake i buf)
      lineNo = genericLength linesBefore ?- 1
      characterNo = case linesBefore of
        (l:_) -> snd $ last linesBefore
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
      Line -> itemsIn s (getCursorRange s cur) splitLines
      ASTNode -> case parseParensE s of
        Right root@(Parens {}) -> case traverseAST (reverse $ map idx cur) root of
          Right (Parens children _ _) -> [0 .. genericLength children]
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

-- moveTo :: String -> Cursor -> Cursor -> Cursor

type Cursor = [CursorNode]

type MetaCursor = [CursorType]

type Cursors = NonEmpty Cursor