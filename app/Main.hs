{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import App
import Control.Lens
import Control.Monad.Loops (iterateM_)
import Cursor
import Data.Default.Class (Default (def))
import Data.List (genericDrop, genericTake, intercalate, sort, sortBy, sortOn)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList, isJust)
import GHC.IO.Handle
import Numeric.Natural (Natural)
import System.Console.ANSI
import System.Directory
import System.Directory.Internal.Prelude (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO
import Util
import Data.Colour
import Data.Colour.SRGB (sRGB, sRGB24show)
import Data.Colour.Names (white, orange, lightblue, darkblue, red)
import Safe
import Data.List.Index (modifyAt)

setup :: IO ()
setup = do
  clearScreen
  putStr "\ESC[6 q" -- Set cursor to non-bliking I-beam, for 0-length cursors
  hideCursor
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

cleanup :: IO ()
cleanup = do
  setup
  putStr "\ESC[0 q" -- Reset cursor
  setSGR [SetDefaultColor Background]
  clearScreen
  showCursor

data AlignPriority = AlignLeft | AlignRight

align :: Natural -> AlignPriority -> a -> [a] -> [a] -> [a]
align size prio filler left right = case prio of
  AlignLeft -> genericTake size left ++ replicate (size ?- l) filler ++ drop (l ?- size) right
  AlignRight -> take (size ?- length right) left ++ replicate (size ?- l) filler ++ genericTake size right
  where
    l = length left + length right

showPos :: (Natural, Natural) -> String
showPos (y, x) = show (y + 1) ++ ":" ++ show (x + 1)

showRanges :: NE.NonEmpty (Natural, Natural) -> String
showRanges = concatMap (\(len, pos) -> show len ++ "[" ++ show pos ++ "]")

statusBarColor :: Colour Float
statusBarColor = sRGB 0.1 0.1 0.2

statusBar :: Natural -> (Natural, Natural) -> App -> [String]
statusBar width viewport App {..} =
  (case message of Just (msg, clr) -> [setSGRCode [ SetRGBColor Background clr, SetDefaultColor Foreground ] ++ align width AlignLeft ' ' msg ""]; _ -> []) ++
  [
    setSGRCode [SetRGBColor Background statusBarColor, SetDefaultColor Foreground] ++ align
      width
      AlignLeft
      ' '
      (show mode ++ "@" ++ show (reverse $ NE.head cursors) ++ "→" ++ showRanges (getCursorRanges contents cursors))
      (show file ++ "@" ++ showPos viewport)
  ]

-- isWithinRange :: Ord a => a -> (a, a) -> Bool
-- isWithinRange value (from, to) = value >= from && value < to

optimalViewport :: (Natural, Natural) -> App -> (Natural, Natural)
optimalViewport (h, w) App {..} = (vy, vx)
  where
    ((y, x), (y', x')) = getCursorAbsolute contents (NE.head cursors)
    vx = max (x ?- w) (x' ?- w)
    vy = ((y + y') `div` 2) ?- (h `div` 2)

relativeCursorPosition :: String -> (Natural, Natural) -> Cursor -> ((Natural, Natural), (Natural, Natural))
relativeCursorPosition buf (vy, vx) cursor =
  let ((y, x), (y_, x_)) = getCursorAbsolute buf cursor
   in ((y ?- vy, x ?- vx), (y_ ?- vy, x_ ?- vx))

data RegionBoundary = Start (Colour Float, Colour Float) (Natural, Natural) | End (Colour Float, Colour Float) (Natural, Natural) deriving (Eq)

instance Ord RegionBoundary where
  Start _ pos < End _ pos' = pos < pos'
  End _ pos < Start _ pos' = pos < pos'
  Start _ pos < Start _ pos' = pos < pos'
  End _ pos < End _ pos' = pos < pos'
  Start _ pos <= End _ pos' = pos <= pos'
  End _ pos <= Start _ pos' = pos <= pos'
  Start _ pos <= Start _ pos' = pos <= pos'
  End _ pos <= End _ pos' = pos <= pos'

getRegionBoundaries :: [((Colour Float, Colour Float), ((Natural, Natural), (Natural, Natural)))] -> [RegionBoundary]
getRegionBoundaries regions = sort $ concatMap (\(c, (s, e)) -> [Start c s, End c e]) regions

colorRegions :: [((Colour Float, Colour Float), ((Natural, Natural), (Natural, Natural)))] -> [String] -> String
colorRegions regions = go boundaries [] 0 0
  where
    boundaries = getRegionBoundaries regions

    go _ _ _ _ [] = setSGRCode [SetDefaultColor Background, SetDefaultColor Foreground]
    go [] _ _ _ ls = intercalate "\n" ls
    go bs@(b : bs') layers y x (s : ls)
      | pos == (y, x) = setSGRCode codes ++ go bs' layers' y x (s : ls)
      | otherwise = ch ++ go bs layers y' x' ls'
      where
        codes = case b of
          Start _ _ -> [SetRGBColor Background bg, SetRGBColor Foreground fg]
          End _ _ -> case layers of
            (colors : (fg', bg') : _) | colors == c -> [SetRGBColor Background bg', SetRGBColor Foreground fg']
            [_] -> [SetDefaultColor Foreground, SetDefaultColor Background]
            _ -> []
        (c@(fg, bg), pos) = case b of
          Start c' p -> (c', p)
          End c' p -> (c', p)
        layers' = case b of
          Start c' _ -> c' : layers
          End c' _ -> remove c' layers
        (ch, ls', y', x') = case s of
          (ch' : s') -> ([ch'], s' : ls, y, x + 1)
          "" -> ("\n", ls, y + 1, 0)

colourForMode :: Mode -> Colour Float
colourForMode Insert = orange
colourForMode _ = white

cursorColor :: App -> Int -> (Colour Float, Colour Float)
cursorColor App {..} n = (case n of 0 -> black; _ -> white, case n of 0 -> colourForMode mode; _ -> let i = 0.2 / (fromIntegral n ** 2) in sRGB i i i)

-- h   j      k     l
-- red yellow green blue
-- peerColors :: [(Colour Float, Colour Float)]
-- peerColors = [(white, sRGB 0.2 0.05 0.05), (white, sRGB 0.2 0.2 0.07), (white, sRGB 0.05 0.2 0.05), (white, sRGB 0.05 0.05 0.2)]

render :: App -> IO ()
render app@App {..} = do
  hideCursor
  size <- getTerminalSize
  case size of
    Nothing -> render app
    Just (h, w) -> do
      let (vy, vx) = optimalViewport (fromIntegral h, fromIntegral w) app
      let content = (\c -> align (fromIntegral w) AlignLeft ' ' c "") . genericDrop vx <$> genericDrop vy (lines contents)
      let activeCursor = NE.head cursors
      let cursorParents = reverse $ take 3 [(cursorColor app n, relativeCursorPosition contents (vy, vx) (drop n activeCursor)) | n <- [0 .. length activeCursor]]
      -- let cursorPeers = zip peerColors [relativeCursorPosition contents (vy, vx) cur | cur <- map (\c -> c contents activeCursor) [prevSibling, (\c s -> nextSibling c $ parent s), (\c s -> prevSibling c $ parent s), nextSibling]]
      let ((y, x), (y', x')) = relativeCursorPosition contents (vy, vx) activeCursor
      -- We're assuming that if y == y', then x < x'
      let coloredContent = lines $ colorRegions cursorParents content
      let !screen = map (clearLineCode ++) $ align (fromIntegral h) AlignRight "" coloredContent (statusBar (fromIntegral w) (vy, vx) app)
      setSGR [SetDefaultColor Background]
      setCursorPosition 0 0
      mapM_ putStrLn (init screen)
      putStr (last screen)
      if y == y' && x == x'
        then do
          putStr $ "\ESC]12;" ++ sRGB24show (colourForMode mode)
          setCursorPosition (fromIntegral y) (fromIntegral x)
          showCursor
        else do
          hideCursor

isSaved :: App -> IO Bool
isSaved App {..} = case file of
  Nothing -> return False
  Just fName -> do
    exists <- doesFileExist fName
    if not exists
      then return False
      else do
        contents' <- readFile fName
        return $ contents' == contents

execute :: App -> String -> IO App
execute app@App {} "q" = do
  saved <- isSaved app
  if saved then cleanup >> exitSuccess else return $ _message ?~ ("There are unsaved changes. Please save them with [:w] or use [:q!]", red) $ app
execute App {} "q!" = cleanup >> exitFailure
execute app@App {..} ('w' : fname) = do
  if not $ null fname
    then writeFile fname contents >> return (_file ?~ fname $ app)
    else case file of
      Just file' -> writeFile file' contents >> return app
      Nothing -> return $ _message ?~ ("No file is currently open. Pass the file name after :w, like [:w<filename>]", red) $ app
execute app@App {} ('e' : fname) = do
  exists <- doesFileExist fname
  contents' <-
    if exists
      then readFile fname
      else return ""
  return $ _contents .~ contents' $ _file ?~ fname $ app
execute app ('@':c) = case readMay ("[" ++ c ++ "]") :: Maybe [Natural] of
  Just is -> return $ _cursors . ix 0 %~ toNthIndicies (reverse is) $ app
  Nothing -> case mapM cursorByChar c :: Maybe MetaCursor of
    Just is -> return $ updateActiveCursorType (toTypes (reverse is)) app
    Nothing -> return $ _message ?~ ("Cursor update not recognized: " <> c, red) $ app
execute app@App {..} ('#':c) = case mapM cursorByChar c :: Maybe MetaCursor of
  Just is -> return $ updateActiveCursorType (\cur -> findCursor contents cur (reverse is)) app
  Nothing -> return $ _message ?~ ("Cursor type set not recongnized: " <> c, red) $ app
execute app c = return $ _message ?~ ("Unknown command: " <> c, red) $ app

handleSequence :: App -> String -> IO App
handleSequence app@(App {..}) c
  | c == "\ESC" && mode == Insert = return $ undoCursor $ _mode .~ Normal $ app
  | c == "\ESC" = return $ _mode .~ Normal $ app
  | c == "\ESC:" = return $ toBestASTNode app
  | c == "\ESC;" = return $ updateActiveCursorType (flip (findCursor contents) [Char, Line]) app 
  | c == "\ESCh" = return $ toPrevTypeSibling app
  | c == "\ESCH" = return $ toFirstTypeSibling app
  | c == "\ESCj" = return $ toChild app
  | c == "\ESCk" = return $ toParent app
  | c == "\ESCl" = return $ toNextTypeSibling app
  | c == "\ESCL" = return $ toLastTypeSibling app
  | c == "\ESCu" = return $ undoCursor app
  | c == "\ESCU" = return $ redoCursor app
  | otherwise = case mode of
      Normal -> return $ case c of
        "\ESC[D" -> toPrevSibling app -- ←
        "\ESC[B" -> toNextCousin app -- ↓
        "\ESC[A" -> toPrevCousin app -- ↑
        "\ESC[C" -> toNextSibling app -- →
        "\ESC[H" -> toFirstSibling app -- Home
        "\ESC[F" -> toLastSibling app -- End
        "h" -> toPrevSibling app
        "H" -> toFirstSibling app
        "j" -> toNextCousin app
        "k" -> toPrevCousin app
        "l" -> toNextSibling app
        "L" -> toLastSibling app
        "i" -> toInsertMode app
        "I" -> toInsertModeAfter app
        "c" -> replace app
        "C" -> replaceToNextSibling app
        "\ESCc" -> toInsertMode $ deleteParent app
        "d" -> delete app
        "\ESCd" -> deleteParent app
        "D" -> deleteToNextSibling app
        -- "\ESCD" -> return $ deleteParentToNextSibling app
        "y" -> yank app
        "Y" -> yankToNextSibling app
        "p" -> paste app
        "P" -> pop app
        "\ESCp" -> dropClipboard app
        "u" -> undo app
        "U" -> redo app
        ":" -> _mode .~ Command "" $ app
        "@" -> _mode .~ Command "@" $ app
        "#" -> _mode .~ Command "#" $ app
        -- "v" -> _mode .~ Select Range (app ^. _cursors . ix 0) $ app
        "`" -> _cursors %~ (\(cur NE.:| rest) -> cur NE.:| (cur : rest)) $ app
        "~" -> _cursors %~ neTailSafe $ app
        _ -> app
      Insert -> return $ case c of
        "\DEL" -> deleteCharacter app
        "\ESC[D" -> toPrevChar app -- ←
        "\ESC[B" -> toNextCousin app -- ↓
        "\ESC[A" -> toPrevCousin app -- ↑
        "\ESC[C" -> toNextChar app -- →
        "\ESC[H" -> toFirstSibling app -- Home
        "\ESC[F" -> toLastChar app -- End
        [ch] -> insert ch app
        _ -> app
      Command command -> case c of
        "\n" -> (_mode .~ Normal) <$> execute app command
        "\DEL" -> return $ _mode .~ Command (init' command) $ app
        [ch] -> return $ _mode .~ Command (command ++ [ch]) $ app
        _ -> return app
      Select Range from -> return $ case c of
        "h" -> toPrevSibling app
        "j" -> toNextCousin app
        "k" -> toPrevCousin app
        "l" -> toNextSibling app
        -- "\n" -> (_mode .~ Normal) . (_cursors . ix 0 %~ (\c -> )) $ app
        _ -> app
      Select Block from -> return $ case c of
        "h" -> toPrevSibling app
        "j" -> toNextCousin app
        "k" -> toPrevCousin app
        "l" -> toNextSibling app
        _ -> app

mainLoop :: App -> IO App
mainLoop app = do
  render app
  getBlockOfChars stdin >>= handleSequence (_message .~ Nothing $ app)

-- From Haskeline
-- Copyright 2007 Judah Jacobson
getBlockOfChars :: Handle -> IO String
getBlockOfChars h = do
  c <- hGetChar h
  loop [c]
  where
    loop cs = do
      isReady <- hReady h
      if not isReady
        then return $ reverse cs
        else do
          c <- hGetChar h
          loop (c : cs)

main :: IO ()
main = do
  setup
  app <-
    getArgs
      >>= ( \case
              [fname] -> do
                exists <- doesFileExist fname
                if exists
                  then do
                    contents' <- readFile fname
                    return $ _contents .~ contents' $ _file ?~ fname $ def
                  else do
                    return $ _file ?~ fname $ def
              _ -> return def
          )
  iterateM_ mainLoop app
