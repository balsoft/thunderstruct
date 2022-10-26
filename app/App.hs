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

data Mode = Normal | Command String | Insert | Replace | Select deriving (Show, Eq)

data App = App
  { contents :: String,
    message :: Maybe String,
    file :: Maybe FilePath,
    mode :: Mode,
    history :: [String],
    historyBack :: [String],
    cursorHistory :: [MetaCursor],
    cursorHistoryBack :: [MetaCursor],
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
        history = [],
        historyBack = [],
        cursorHistory = [],
        cursorHistoryBack = [],
        clipboard = [],
        cursors = [] :| []
      }

$(makeLensesFor (map (\name -> (name, '_':name)) ["contents", "message", "file", "mode", "history", "historyBack", "cursorHistory", "cursorHistoryBack", "clipboard", "cursors"]) ''App)

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

toPrevTypeSibling :: App -> App
toPrevTypeSibling app@App {..} = fixMode $ _cursors . ix 0 %~ (\c -> findCursor contents c (metaCursor $ updateCursor c $ modifyAt 0 prevType c)) $ saveCursorHistory app

toNextTypeSibling :: App -> App
toNextTypeSibling app@App {..} = fixMode $ _cursors . ix 0 %~ (\c -> findCursor contents c (metaCursor $ updateCursor c $ modifyAt 0 nextType c)) $ saveCursorHistory app

toFirstTypeSibling :: App -> App
toFirstTypeSibling app@App {..} = fixMode $ _cursors . ix 0 %~ (\c -> findCursor contents c (metaCursor $ updateCursor c $ modifyAt 0 (_cursorType .~ minBound) c)) $ saveCursorHistory app

toLastTypeSibling :: App -> App
toLastTypeSibling app@App {..} = fixMode $ _cursors . ix 0 %~ (\c -> findCursor contents c (metaCursor $ updateCursor c $ modifyAt 0 (_cursorType .~ maxBound ) c)) $ saveCursorHistory app

toBestASTNode :: App -> App
toBestASTNode app@App {..} = fixMode $ _cursors . ix 0 %~ bestASTNode contents $ saveCursorHistory app

toInsertMode :: App -> App
toInsertMode =  charCursor . (_mode .~ Insert) . saveHistory

fixCursor :: App -> App
fixCursor app@App {..} = _cursors . ix 0 %~ (\c -> coerceCursor contents c c) $ app

fixCursorAfter :: (App -> App) -> App -> App
fixCursorAfter trans app = let app' = trans app in _cursors . ix 0 %~ (\c -> coerceCursor (app' ^. _contents) [CN Char (getCharacterPos (app ^. _contents) c)] c) $ app'

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


deleteUnderCursor' :: App -> App
deleteUnderCursor' app@App {..} = _contents %~ deleteMany (getCursorRange contents (NE.head cursors)) $ app

deleteUnderCursor :: App -> App
deleteUnderCursor = fixCursorAfter deleteUnderCursor' . saveHistory

deleteToNextSibling' :: App -> App
deleteToNextSibling' app@App {..} = _contents %~ deleteMany (pos, pos' - pos) $ app
  where
    (pos, _) = getCursorRange contents (NE.head cursors)
    (pos', _) = getCursorRange contents $ nextSibling contents (NE.head cursors)

deleteToNextSibling :: App -> App
deleteToNextSibling = fixCursorAfter deleteToNextSibling' . saveHistory

deleteParent :: App -> App
deleteParent app@App {..} = fixCursorAfter (_contents %~ deleteMany (getCursorRange contents $ drop 1 (NE.head cursors))) $ saveHistory app

deleteCharacter :: App -> App
deleteCharacter app@App {..} = _cursors . ix 0 %~ (\c -> coerceCursor contents' [CN Char (getCharacterPos contents c ?- (1 :: Int))] c) $ _contents .~ contents' $ app
  where contents' = deleteAt (getCharacterPos contents (NE.head cursors) ?- (1 :: Int) :: Int) contents

replaceUnderCursor :: App -> App
replaceUnderCursor app = _contents .~ (deleteUnderCursor' app ^. _contents) $ toInsertMode app

replaceToNextSibling :: App -> App
replaceToNextSibling app = _contents .~ (deleteToNextSibling' app ^. _contents) $ toInsertMode app

-- yank :: App -> IO App
-- yank app@App {..} = callProcess "wl-copy" [slice (getCursorRange _contents (NE.head __cursors)) _contents] `catch` (\(_ :: IOError) -> return ()) >> pure app

-- paste :: App -> IO App
-- paste app@App {..} = (\clipboard -> contents %~ (insertMany (getCharacterPos _contents (NE.head __cursors)) clipboard . deleteMany (getCursorRange _contents (NE.head __cursors))) $ saveHistory app) <$> readProcess "wl-paste" ["-n"] ""

yank :: App -> App
yank app@App {..} = _clipboard %~ (slice (getCursorRange contents (NE.head cursors)) contents :) $ app

dropClipboard :: App -> App
dropClipboard = _clipboard %~ drop 1

paste :: App -> App
paste app@App {clipboard = entry:_, ..} = _contents %~ insertMany (getCharacterPos contents (NE.head (app ^. _cursors))) entry $ charCursor $ saveHistory app
paste app = app

pop :: App -> App
pop app@App {clipboard = entry:rest, ..} = _clipboard .~ rest $ _contents %~ insertMany (getCharacterPos contents (NE.head (app ^. _cursors))) entry $ charCursor $ saveHistory app
pop app = app

undo :: App -> App
undo app@App {..} = case history of
  entry:rest -> _contents .~ entry $ _history .~ rest $ _historyBack %~ (contents :) $ app
  _ -> app

redo :: App -> App
redo app@App {..} = case historyBack of
  entry:rest -> _contents .~ entry $ _historyBack .~ rest $ _history %~ (contents :) $ app
  _ -> app

undoCursor :: App -> App
undoCursor app@App {..} = case cursorHistory of
  entry:rest -> fixMode $ _cursors . ix 0 %~ (\c -> findCursor contents c entry) $ _cursorHistory .~ rest $ _cursorHistoryBack %~ (metaCursor (NE.head cursors) :) $ app
  _ -> app

redoCursor :: App -> App
redoCursor app@App {..} = case cursorHistoryBack of
  entry:rest -> fixMode $ _cursors . ix 0 %~ (\c -> findCursor contents c entry) $ _cursorHistoryBack .~ rest $ _cursorHistory %~ (metaCursor (NE.head cursors) :) $ app
  _ -> app

saveHistory :: App -> App
saveHistory app@App {..} = _historyBack .~ [] $ _history %~ (contents :) $ app

saveCursorHistory :: App -> App
saveCursorHistory app@App {..} = _cursorHistoryBack .~ [] $ _cursorHistory %~ (metaCursor (NE.head cursors) :) $ app