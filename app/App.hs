{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module App where

import Control.Lens
import Cursor
import Data.Default.Class
import Data.List.Index (modifyAt)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Util
import Prelude hiding (Char, Word)
import qualified Prelude
import Numeric.Natural (Natural)
import Data.Colour (Colour)

data Selection = Range | Block deriving (Eq, Show)

data Mode = Normal | Command String | Insert | Select Selection Cursor deriving (Show, Eq)

data App = App
  { contents :: String,
    message :: Maybe (String, Colour Float),
    file :: Maybe FilePath,
    mode :: Mode,
    history :: History String,
    cursorHistory :: History MetaCursor,
    clipboard :: [String],
    cursors :: Cursors
  }
  deriving (Show)

instance Default App where
  def =
    App
      { contents = "",
        message = Nothing,
        file = Nothing,
        mode = Normal,
        history = def,
        cursorHistory = def,
        clipboard = [],
        cursors = [] :| []
      }

$(makeLensesFor (map (\name -> (name, '_':name)) ["contents", "message", "file", "mode", "history", "cursorHistory", "clipboard", "cursors"]) ''App)

toNextSibling :: App -> App
toNextSibling app@App {..} = _cursors . ix 0 %~ nextSibling contents $ app

toPrevSibling :: App -> App
toPrevSibling app@App {..} = _cursors . ix 0 %~ prevSibling contents $ app

toFirstSibling :: App -> App
toFirstSibling app@App {..} = _cursors . ix 0 %~ firstSibling contents $ app

toLastSibling :: App -> App
toLastSibling app@App {..} = _cursors . ix 0 %~ lastSibling contents $ app

toNextCousin :: App -> App
toNextCousin app@App {..} = _cursors . ix 0 %~ nextCousin contents $ app

toPrevCousin :: App -> App
toPrevCousin app@App {..} = _cursors . ix 0 %~ prevCousin contents $ app

toParent :: App -> App
toParent = fixMode . (_cursors . ix 0 %~ drop 1) . saveCursorHistory

toChild :: App -> App
toChild app@App {..} = _cursors . ix 0 %~ firstChild contents $ saveCursorHistory app

-- updateActiveCursor :: (Cursor -> Cursor) -> App -> App
-- updateActiveCursor action app@App {..} = _cursors . ix 0 %~ (\c -> findCursor contents (action c) (metaCursor c)) $ saveCursorHistory app

updateActiveCursorType :: (Cursor -> Cursor) -> App -> App
updateActiveCursorType action app@App {..} = fixMode $ _cursors . ix 0 %~ (\c -> findCursor contents c (metaCursor $ updateCursor c $ action c)) $ saveCursorHistory app

toPrevTypeSibling :: App -> App
toPrevTypeSibling app@App {..} = updateActiveCursorType (prevTypeSibling contents) $ app

toNextTypeSibling :: App -> App
toNextTypeSibling app@App {..} = updateActiveCursorType (nextTypeSibling contents) $ app

toFirstTypeSibling :: App -> App
toFirstTypeSibling = updateActiveCursorType (modifyAt 0 (_cursorType .~ minBound))

toLastTypeSibling :: App -> App
toLastTypeSibling = updateActiveCursorType (modifyAt 0 (_cursorType .~ maxBound))

toNthTypeSibling :: Natural -> App -> App
toNthTypeSibling n = updateActiveCursorType (modifyAt 0 (toNthType n))

toBestASTNode :: App -> App
toBestASTNode app@App {..} = updateActiveCursorType (bestASTNode contents) app

toInsertMode :: App -> App
toInsertMode = charCursor . (_mode .~ Insert) . saveHistory

toInsertModeAfter :: App -> App
toInsertModeAfter app@App {..} = (_cursors . ix 0 %~ (ix 0 %~ (_idx .~ uncurry (+) (getCursorRange contents (NE.head cursors))))) . charCursor . (_mode .~ Insert) . saveHistory . saveCursorHistory $ app

fixCursor :: App -> App
fixCursor app@App {..} = _cursors . ix 0 %~ (\c -> coerceCursor contents c c) $ app

fixCursorsAfter :: (App -> App) -> App -> App
fixCursorsAfter trans app = let app' = trans app in _cursors %~ fmap (\c -> coerceCursor (app' ^. _contents) [CN Char (getCharacterPos (app ^. _contents) c)] c) $ app'

fixMode :: App -> App
fixMode app@App { cursors = ((CN Char _):_):|_, mode = Insert } = app
fixMode app@App { mode = Insert } = _mode .~ Normal $ app -- Insert mode is only available with a char cursor
fixMode app = app

insertAfter :: Prelude.Char -> App -> App
insertAfter c app@App {..} = _contents %~ insertAt (getCharacterPos contents (NE.head cursors)) c $ app

charCursor :: App -> App
charCursor app@App {..} = (_cursors %~ (\c -> case c of ((CN Char _) : _) :| _ -> c; (other:rest) :| rest' -> coerceCursor contents (other:rest) (CN Char 0 : rest) :| rest'; _ -> c)) $ saveCursorHistory app

insert :: Prelude.Char -> App -> App
insert char app@App {..} = _cursors . ix 0 %~ (\c -> coerceCursor contents' [CN Char (getCharacterPos contents c + 1)] c) $ _contents .~ contents' $ app
  where contents' = insertAt (getCharacterPos contents (NE.head cursors)) char contents


delete' :: App -> App
delete' app@App {..} = _contents %~ deleteMultiple (NE.toList (getCursorRanges contents cursors)) $ app

delete :: App -> App
delete = fixCursorsAfter delete' . saveHistory

deleteToNextSibling' :: App -> App
deleteToNextSibling' app@App {..} = _contents %~ deleteMultiple (getCursorToNextSiblingRanges contents cursors) $ app

deleteToNextSibling :: App -> App
deleteToNextSibling = fixCursorsAfter deleteToNextSibling' . saveHistory

deleteParent :: App -> App
deleteParent app@App {..} = fixCursorsAfter (_contents %~ deleteMany (getCursorRange contents $ drop 1 (NE.head cursors))) $ saveHistory app

deleteCharacter :: App -> App
deleteCharacter app@App {..} = _cursors . ix 0 %~ (\c -> coerceCursor contents' [CN Char (getCharacterPos contents c ?- (1 :: Int))] c) $ _contents .~ contents' $ app
  where contents' = deleteAt (getCharacterPos contents (NE.head cursors) ?- (1 :: Int) :: Int) contents

replace :: App -> App
replace app = _contents .~ (delete' app ^. _contents) $ toInsertMode app

replaceToNextSibling :: App -> App
replaceToNextSibling app = _contents .~ (deleteToNextSibling' app ^. _contents) $ toInsertMode app

-- yank :: App -> IO App
-- yank app@App {..} = callProcess "wl-copy" [slice (getCursorRange _contents (NE.head __cursors)) _contents] `catch` (\(_ :: IOError) -> return ()) >> pure app

-- paste :: App -> IO App
-- paste app@App {..} = (\clipboard -> contents %~ (insertMany (getCharacterPos _contents (NE.head __cursors)) clipboard . deleteMany (getCursorRange _contents (NE.head __cursors))) $ saveHistory app) <$> readProcess "wl-paste" ["-n"] ""

yank :: App -> App
yank app@App {..} = _clipboard %~ (slice (getCursorRange contents (NE.head cursors)) contents :) $ app

yankToNextSibling :: App -> App
yankToNextSibling app@App {..} = _clipboard %~ (slice (pos, pos' - pos) contents :) $ app
  where
    (pos, _) = getCursorRange contents (NE.head cursors)
    (pos', _) = getCursorRange contents $ nextSibling contents (NE.head cursors)

dropClipboard :: App -> App
dropClipboard = _clipboard %~ drop 1

paste :: App -> App
paste app@App {clipboard = entry:_, ..} = _contents %~ insertMany (getCharacterPos contents (NE.head (app ^. _cursors))) entry $ charCursor $ saveHistory app
paste app = app

pop :: App -> App
pop app@App {clipboard = entry:rest, ..} = _clipboard .~ rest $ _contents %~ insertMany (getCharacterPos contents (NE.head (app ^. _cursors))) entry $ charCursor $ saveHistory app
pop app = app

undo :: App -> App
undo app@App {..} = _contents .? currentHistItem history $ _history %~ turnBackTime $ app

redo :: App -> App
redo app@App {..} = _contents .? nextHistItem history $ _history %~ backToTheFuture $ app

undoCursor :: App -> App
undoCursor app@App {..} = fixMode $ (_cursors . ix 0) %? fmap (flip (findCursor contents)) (currentHistItem cursorHistory) $ _cursorHistory %~ turnBackTime $ app

redoCursor :: App -> App
redoCursor app@App {..} = fixMode $ (_cursors . ix 0) %? fmap (flip (findCursor contents)) (nextHistItem cursorHistory) $ _cursorHistory %~ backToTheFuture $ app

(?:) :: Eq a => a -> [a] -> [a]
a ?: [] = [a]
a ?: as@(a':_)
  | a /= a' = a:as
  | otherwise = as

saveHistory :: App -> App
saveHistory app@App {..} = _history %~ newItem contents  $ app

saveCursorHistory :: App -> App
saveCursorHistory app@App {..} = _cursorHistory %~ newItem (metaCursor $ NE.head cursors) $ app