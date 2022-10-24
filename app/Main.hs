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

setup :: IO ()
setup = do
  clearScreen
  hideCursor
  hSetEcho stdin False
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

cleanup :: IO ()
cleanup = do
  setup
  setSGR [SetDefaultColor Background]
  clearScreen
  showCursor

data AlignPriority = AlignLeft | AlignRight

align :: Natural -> AlignPriority -> a -> [a] -> [a] -> [a]
align size prio filler left right = case prio of
  AlignLeft -> genericTake size left ++ replicate (size ?- l) filler ++ drop (length left ?- size) right
  AlignRight -> take (size ?- length right) left ++ replicate (size ?- l) filler ++ genericTake size right
  where
    l = length left + length right

showPos :: (Natural, Natural) -> String
showPos (y, x) = show (y + 1) ++ ":" ++ show (x + 1)

showRanges :: NE.NonEmpty (Natural, Natural) -> String
showRanges = concatMap (\(len, pos) -> show len ++ "[" ++ show pos ++ "]")

statusBar :: Natural -> (Natural, Natural) -> App -> [String]
statusBar width viewport app@App {..} =
  [ align width AlignLeft ' ' (setSGRCode [SetPaletteColor Background 235, SetDefaultColor Foreground] ++ concat (maybeToList (app ^. message))) "\n",
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

data RegionBoundary = Start (Word8, Word8) (Natural, Natural) | End (Word8, Word8) (Natural, Natural) deriving (Eq)

instance Ord RegionBoundary where
  Start _ pos < End _ pos' = pos < pos'
  End _ pos < Start _ pos' = pos < pos'
  Start _ pos < Start _ pos' = pos < pos'
  End _ pos < End _ pos' = pos < pos'
  Start _ pos <= End _ pos' = pos <= pos'
  End _ pos <= Start _ pos' = pos <= pos'
  Start _ pos <= Start _ pos' = pos <= pos'
  End _ pos <= End _ pos' = pos <= pos'

getRegionBoundaries :: [((Word8, Word8), ((Natural, Natural), (Natural, Natural)))] -> [RegionBoundary]
getRegionBoundaries regions = sort $ concatMap (\(c, (s, e)) -> [Start c s, End c e]) regions

colorRegions :: [((Word8, Word8), ((Natural, Natural), (Natural, Natural)))] -> [String] -> String
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
          Start _ _ -> [SetPaletteColor Background bg, SetPaletteColor Foreground fg]
          End _ _ -> case layers of
            (colors : (fg', bg') : _) | colors == c -> [SetPaletteColor Background bg', SetPaletteColor Foreground fg']
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
      let cursorParents = reverse $ take 3 [((case n of 0 -> 0; _ -> 255, case n of 0 -> 255; 1 -> 236; 2 -> 233; _ -> 0), relativeCursorPosition _contents (vy, vx) (drop n activeCursor)) | n <- [0 .. length activeCursor]]
      let ((y, x), (y', x')) = relativeCursorPosition _contents (vy, vx) activeCursor
      -- We're assuming that if y == y', then x < x'
      let coloredContent = lines $ colorRegions cursorParents content
      let !screen = align (fromIntegral h + 2) AlignRight ("\n" <> clearLineCode) coloredContent (statusBar (fromIntegral w) (vy, vx) app)
      setSGR [SetDefaultColor Background]
      setCursorPosition 0 0
      mapM_ putStr screen
      if y == y' && x == x'
        then do
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
  | c == "\ESC[D" = return $ prevSibling app -- ←
  | c == "\ESC[B" = return $ nextCousin app -- ↓
  | c == "\ESC[A" = return $ prevCousin app -- ↑
  | c == "\ESC[C" = return $ nextSibling app -- →
  | c == "\ESC[H" = return $ firstSibling app -- Home
  | c == "\ESC[F" = return $ lastSibling app -- End
  -- \| c == '\DEL' = return $ deleteUnderCursor app
  | _mode == Normal = case c of
      "\ESCh" -> return $ prevTypeSibling app
      "\ESCj" -> return $ child app
      "\ESCk" -> return $ parent app
      "\ESCl" -> return $ nextTypeSibling app
      "h" -> return $ prevSibling app
      "j" -> return $ nextCousin app
      "k" -> return $ prevCousin app
      "l" -> return $ nextSibling app
      "i" -> return $ saveHistory $ insertMode app
      "d" -> return $ deleteUnderCursor $ saveHistory app
      "y" -> yank app
      "p" -> paste app
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
