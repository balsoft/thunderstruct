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
import Data.List (genericDrop, genericTake, intercalate, sort)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (maybeToList)
import Data.Word (Word8)
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
import Data.Colour.Names (white, green, orange)

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

statusBar :: Natural -> (Natural, Natural) -> App -> [String]
statusBar width viewport app@App {..} =
  [ setSGRCode [SetPaletteColor Background 235, SetDefaultColor Foreground] ++ align width AlignLeft ' ' (concat (maybeToList (app ^. message))) "",
    align
      width
      AlignLeft
      ' '
      (show (app ^. mode) ++ "@" ++ show (reverse $ NE.head _cursors) ++ "→" ++ showRanges (getCursorRanges _contents _cursors))
      (show (app ^. file) ++ "@" ++ showPos viewport)
  ]

-- isWithinRange :: Ord a => a -> (a, a) -> Bool
-- isWithinRange value (from, to) = value >= from && value < to

optimalViewport :: (Natural, Natural) -> App -> (Natural, Natural)
optimalViewport (h, w) App {..} = (vy, vx)
  where
    ((y, x), (y', x')) = getCursorAbsolute _contents (NE.head _cursors)
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

    go _ _ _ _ [] = ""
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
cursorColor App {..} n = (case n of 0 -> black; _ -> white, case n of 0 -> colourForMode _mode; 1 -> sRGB 0.15 0.15 0.15; 2 -> sRGB 0.05 0.05 0.05; _ -> black)

render :: App -> IO ()
render app@App {..} = do
  hideCursor
  size <- getTerminalSize
  case size of
    Nothing -> render app
    Just (h, w) -> do
      let (vy, vx) = optimalViewport (fromIntegral h, fromIntegral w) app
      let content = (\c -> align (fromIntegral w) AlignLeft ' ' c "") . genericDrop vx <$> genericDrop vy (lines (app ^. contents))
      let activeCursor = NE.head _cursors
      let cursorParents = reverse $ take 3 [(cursorColor app n, relativeCursorPosition _contents (vy, vx) (drop n activeCursor)) | n <- [0 .. length activeCursor]]
      let ((y, x), (y', x')) = relativeCursorPosition _contents (vy, vx) activeCursor
      -- We're assuming that if y == y', then x < x'
      let coloredContent = lines $ colorRegions cursorParents content
      let !screen = align (fromIntegral h) AlignRight ("\n" <> clearLineCode) coloredContent (statusBar (fromIntegral w) (vy, vx) app)
      setSGR [SetDefaultColor Background]
      setCursorPosition 0 0
      mapM_ putStr screen
      if y == y' && x == x'
        then do
          putStr $ "\ESC]12;" ++ sRGB24show (colourForMode _mode)
          setCursorPosition (fromIntegral y) (fromIntegral x)
          showCursor
        else do
          hideCursor

isSaved :: App -> IO Bool
isSaved App {..} = case _file of
  Nothing -> return False
  Just fName -> do
    exists <- doesFileExist fName
    if not exists
      then return False
      else do
        contents' <- readFile fName
        return $ contents' == _contents

execute :: App -> String -> IO App
execute app@App {} "q" = do
  saved <- isSaved app
  if saved then cleanup >> exitSuccess else return $ message ?~ "There are unsaved changes. Please save them with [:w] or use [:q!]" $ app
execute App {} "q!" = cleanup >> exitFailure
execute app@App {..} ('w' : fname) = do
  if not $ null fname
    then writeFile fname _contents >> return (file ?~ fname $ app)
    else case _file of
      Just file' -> writeFile file' _contents >> return app
      Nothing -> return $ message ?~ "No file is currently open. Pass the file name after :w, like [:w<filename>]" $ app
execute app@App {} ('e' : fname) = do
  exists <- doesFileExist fname
  contents' <-
    if exists
      then readFile fname
      else return ""
  return $ contents .~ contents' $ file ?~ fname $ app
execute app c = return $ message ?~ "Unknown command: " <> c $ app

handleSequence :: App -> String -> IO App
handleSequence app@(App {..}) c
  | c == "\ESC" = return $ mode .~ Normal $ app
  | c == "\ESC[D" = return $ toPrevSibling app -- ←
  | c == "\ESC[B" = return $ toNextCousin app -- ↓
  | c == "\ESC[A" = return $ toPrevCousin app -- ↑
  | c == "\ESC[C" = return $ toNextSibling app -- →
  | c == "\ESC[H" = return $ toFirstSibling app -- Home
  | c == "\ESC[F" = return $ toLastSibling app -- End
  | c == "\ESC:" = return $ toBestASTNode app
  | c == "\ESCh" = return $ toPrevTypeSibling app
  | c == "\ESCH" = return $ toFirstTypeSibling app
  | c == "\ESCj" = return $ toChild app
  | c == "\ESCk" = return $ toParent app
  | c == "\ESCl" = return $ toNextTypeSibling app
  | c == "\ESCL" = return $ toLastTypeSibling app
  | c == "\ESCu" = return $ undoCursor app
  | c == "\ESCU" = return $ redoCursor app
  -- \| c == '\DEL' = return $ deleteUnderCursor app
  | _mode == Normal = case c of
      "h" -> return $ toPrevSibling app
      "j" -> return $ toNextCousin app
      "k" -> return $ toPrevCousin app
      "l" -> return $ toNextSibling app
      "i" -> return $ toInsertMode app
      "I" -> return $ toLastSibling $ toInsertMode app
      "c" -> return $ toInsertMode $ deleteUnderCursor app
      "C" -> return $ toInsertMode $ deleteToNextSibling app
      "\ESCc" -> return $ toInsertMode $ deleteParent app
      "d" -> return $ deleteUnderCursor app
      "\ESCd" -> return $ deleteParent app
      "D" -> return $ deleteToNextSibling app
      -- "\ESCD" -> return $ deleteParentToNextSibling app
      "y" -> return $ yank app
      "Y" -> return $ dropClipboard app
      "p" -> return $ paste app
      "P" -> return $ pop app
      "u" -> return $ undo app
      "U" -> return $ redo app
      ":" -> return $ mode .~ Command "" $ app
      _ -> return app
  | _mode == Insert = return $ case c of
      "\DEL" -> deleteCharacter app
      [ch] -> insert ch app
      _ -> app
  | otherwise = case _mode of
      Command command -> case c of
        "\n" -> (mode .~ Normal) <$> execute app command
        "\DEL" -> return $ mode .~ Command (init' command) $ app
        [ch] -> return $ mode .~ Command (command ++ [ch]) $ app
        _ -> return app
      _ -> return app

mainLoop :: App -> IO App
mainLoop app = do
  render app
  getBlockOfChars stdin >>= handleSequence (message .~ Nothing $ app)

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
                    return $ contents .~ contents' $ file ?~ fname $ def
                  else do
                    return $ file ?~ fname $ def
              _ -> return def
          )
  iterateM_ mainLoop app
