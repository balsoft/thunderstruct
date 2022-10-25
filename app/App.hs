{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module App where

import Control.Exception (catch)
import Control.Lens
import Cursor
import Data.Default.Class
import Data.List (genericLength)
import Data.List.Index (modifyAt, setAt)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import System.Process (callProcess, readProcess)
import Util
import Prelude hiding (Char, Word)
import qualified Prelude

data Mode = Normal | Command String | Insert | Replace | Select Cursors Cursors deriving (Show, Eq)

data App = App
  { _contents :: String,
    _message :: Maybe String,
    _file :: Maybe FilePath,
    _mode :: Mode,
    _history :: [String],
    _historyBack :: [String],
    _cursorHistory :: [MetaCursor],
    _cursorHistoryBack :: [MetaCursor],
    _clipboard :: [String],
    _cursors :: Cursors
  }
  deriving (Show)

instance Default App where
  def =
    App
      { _contents = "",
        _message = Nothing,
        _file = Nothing,
        _mode = Normal,
        _history = [],
        _historyBack = [],
        _cursorHistory = [],
        _cursorHistoryBack = [],
        _clipboard = [],
        _cursors = [] :| []
      }

$(makeLenses ''App)

toNextSibling :: App -> App
toNextSibling app@App {..} = cursors . ix 0 %~ nextSibling _contents $ app

toPrevSibling :: App -> App
toPrevSibling app@App {..} = cursors . ix 0 %~ prevSibling _contents $ app

toFirstSibling :: App -> App
toFirstSibling app@App {..} = cursors . ix 0 %~ firstSibling _contents $ app

toLastSibling :: App -> App
toLastSibling app@App {..} = cursors . ix 0 %~ lastSibling _contents $ app

toNextCousin :: App -> App
toNextCousin app@App {..} = cursors . ix 0 %~ nextCousin _contents $ app

toPrevCousin :: App -> App
toPrevCousin app@App {..} = cursors . ix 0 %~ prevCousin _contents $ app

toParent :: App -> App
toParent = fixMode . (cursors . ix 0 %~ drop 1) . saveCursorHistory

toChild :: App -> App
toChild app@App {..} = cursors . ix 0 %~ firstChild _contents $ saveCursorHistory app

toPrevTypeSibling :: App -> App
toPrevTypeSibling app@App {..} = fixMode $ cursors . ix 0 %~ (\c -> findCursor _contents c (metaCursor $ updateCursor c $ modifyAt 0 prevType c)) $ saveCursorHistory app

toNextTypeSibling :: App -> App
toNextTypeSibling app@App {..} = fixMode $ cursors . ix 0 %~ (\c -> findCursor _contents c (metaCursor $ updateCursor c $ modifyAt 0 nextType c)) $ saveCursorHistory app

toFirstTypeSibling :: App -> App
toFirstTypeSibling app@App {..} = fixMode $ cursors . ix 0 %~ (\c -> findCursor _contents c (metaCursor $ updateCursor c $ modifyAt 0 (\c -> c { cursorType = minBound }) c)) $ saveCursorHistory app

toLastTypeSibling :: App -> App
toLastTypeSibling app@App {..} = fixMode $ cursors . ix 0 %~ (\c -> findCursor _contents c (metaCursor $ updateCursor c $ modifyAt 0 (\c -> c { cursorType = maxBound }) c)) $ saveCursorHistory app

toBestASTNode :: App -> App
toBestASTNode app@App {..} = fixMode $ cursors . ix 0 %~ bestASTNode _contents $ saveCursorHistory app

toInsertMode :: App -> App
toInsertMode =  charCursor . (mode .~ Insert) . saveHistory

fixCursor :: App -> App
fixCursor app@App {..} = cursors . ix 0 %~ (\c -> coerceCursor _contents c c) $ app

fixMode :: App -> App
fixMode app@App { _cursors = ((CN Char _):_):|_, _mode = Insert } = app
fixMode app@App { _mode = Insert } = mode .~ Normal $ app -- Insert mode is only available with a char cursor
fixMode app = app

insertAfter :: Prelude.Char -> App -> App
insertAfter c app@App {..} = contents %~ insertAt (getCharacterPos _contents (NE.head _cursors)) c $ app

charCursor :: App -> App
charCursor = cursors %~ (\c -> case c of ((CN Char _) : _) :| _ -> c; old :| rest -> (CN Char 0 : old) :| rest)

insert :: Prelude.Char -> App -> App
insert char app@App {..} = cursors . ix 0 %~ (\c -> coerceCursor contents' [CN Char (getCharacterPos _contents c + 1)] c) $ contents .~ contents' $ app
  where contents' = insertAt (getCharacterPos _contents (NE.head _cursors)) char _contents

deleteUnderCursor :: App -> App
deleteUnderCursor app@App {..} = fixCursor $ contents %~ deleteMany (getCursorRange _contents (NE.head _cursors)) $ saveHistory app

deleteToNextSibling :: App -> App
deleteToNextSibling app@App {..} = fixCursor $ contents %~ deleteMany (pos, pos' - pos) $ saveHistory app
  where
    (pos, _) = getCursorRange _contents (NE.head _cursors)
    (pos', _) = getCursorRange _contents $ modifyAt 0 (>+ 1) (NE.head _cursors)

deleteParent :: App -> App
deleteParent app@App {..} = fixCursor $ contents %~ deleteMany (getCursorRange _contents $ drop 1 (NE.head _cursors)) $ saveHistory app

deleteCharacter :: App -> App
deleteCharacter app@App {..} = cursors . ix 0 %~ (\c -> coerceCursor contents' [CN Char (getCharacterPos _contents c ?- (1 :: Int))] c) $ contents .~ contents' $ app
  where contents' = deleteAt (getCharacterPos _contents (NE.head _cursors) ?- (1 :: Int) :: Int) _contents

-- yank :: App -> IO App
-- yank app@App {..} = callProcess "wl-copy" [slice (getCursorRange _contents (NE.head _cursors)) _contents] `catch` (\(_ :: IOError) -> return ()) >> pure app

-- paste :: App -> IO App
-- paste app@App {..} = (\clipboard -> contents %~ (insertMany (getCharacterPos _contents (NE.head _cursors)) clipboard . deleteMany (getCursorRange _contents (NE.head _cursors))) $ saveHistory app) <$> readProcess "wl-paste" ["-n"] ""

yank :: App -> App
yank app@App {..} = clipboard %~ (slice (getCursorRange _contents (NE.head _cursors)) _contents :) $ app

dropClipboard :: App -> App
dropClipboard = clipboard %~ drop 1

paste :: App -> App
paste app@App {_clipboard = entry:_, ..} = contents %~ insertMany (getCharacterPos _contents (NE.head (app ^. cursors))) entry $ charCursor $ saveHistory app
paste app = app

pop :: App -> App
pop app@App {_clipboard = entry:rest, ..} = clipboard .~ rest $ contents %~ insertMany (getCharacterPos _contents (NE.head (app ^. cursors))) entry $ charCursor $ saveHistory app
pop app = app

undo :: App -> App
undo app@App {..} = case _history of
  entry:rest -> contents .~ entry $ history .~ rest $ historyBack %~ (_contents :) $ app
  _ -> app

redo :: App -> App
redo app@App {..} = case _historyBack of
  entry:rest -> contents .~ entry $ historyBack .~ rest $ history %~ (_contents :) $ app
  _ -> app

undoCursor :: App -> App
undoCursor app@App {..} = case _cursorHistory of
  entry:rest -> fixMode $ cursors . ix 0 %~ (\c -> findCursor _contents c entry) $ cursorHistory .~ rest $ cursorHistoryBack %~ (metaCursor (NE.head _cursors) :) $ app
  _ -> app

redoCursor :: App -> App
redoCursor app@App {..} = case _cursorHistoryBack of
  entry:rest -> fixMode $ cursors . ix 0 %~ (\c -> findCursor _contents c entry) $ cursorHistoryBack .~ rest $ cursorHistory %~ (metaCursor (NE.head _cursors) :) $ app
  _ -> app

saveHistory :: App -> App
saveHistory app@App {..} = historyBack .~ [] $ history %~ (_contents :) $ app

saveCursorHistory :: App -> App
saveCursorHistory app@App {..} = cursorHistoryBack .~ [] $ cursorHistory %~ (metaCursor (NE.head _cursors) :) $ app