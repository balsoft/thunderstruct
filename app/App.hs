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
import Data.List.Index (modifyAt)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import System.Process (callProcess, readProcess)
import Util
import Prelude hiding (Char, Word)
import qualified Prelude

-- data SelectionType = Area | Index | Lines deriving (Show)

data Mode = Normal | Command String | Insert | Replace | Select Cursors Cursors deriving (Show, Eq)

data App = App
  { _contents :: String,
    _message :: Maybe String,
    _file :: Maybe FilePath,
    _mode :: Mode,
    _history :: [String],
    _historyBack :: [String],
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
        _cursors = [] :| []
      }

$(makeLenses ''App)

-- fixCursor :: App -> ApV
-- fixCursor app@App {..} = cursors . ix 0 %~ (\cur -> case cur of (CN {..}):cs | idx > genericLength (childrenOfType _contents cs cursorType) -> CN (genericLength (childrenOfType _contents cs cursorType)) cursorType : cs; _ -> cur)

nextSibling :: App -> App
nextSibling app@App {..} = fixCursor $ cursors . ix 0 %~ modifyAt 0 (>+ 1) $ app

prevSibling :: App -> App
prevSibling app@App {..} = fixCursor $ cursors . ix 0 %~  modifyAt 0 (>- 1) $ app

firstSibling :: App -> App
firstSibling = fixCursor . (cursors . ix 0 %~ modifyAt 0 (\x -> x { idx = 0 }))

lastSibling :: App -> App
lastSibling app@App {..} = cursors . ix 0 %~ modifyAt 0 (\x -> x { idx = genericLength (childrenOfType _contents (tail $ NE.head _cursors) (cursorType $ head $ NE.head _cursors)) ?- 1 }) $ app

nextCousin :: App -> App
nextCousin = fixCursor . (cursors . ix 0 %~ modifyAt 1 (>+ 1))

prevCousin :: App -> App
prevCousin = fixCursor . (cursors . ix 0 %~ modifyAt 1 (>- 1))

parent :: App -> App
parent = cursors . ix 0 %~ drop 1

child :: App -> App
child app@App {..} = cursors . ix 0 %~ firstChild _contents $ app

prevTypeSibling :: App -> App
prevTypeSibling app@App {..} = cursors . ix 0 %~ (\c -> findCursor _contents c (metaCursor $ updateCursor c $ modifyAt 0 prevType c)) $ app

nextTypeSibling :: App -> App
nextTypeSibling app@App {..} = cursors . ix 0 %~ (\c -> findCursor _contents c (metaCursor $ updateCursor c $ modifyAt 0 nextType c)) $ app

insertMode :: App -> App
insertMode = (cursors %~ (\c -> case c of ((CN Char _) : _) :| _ -> c; old :| rest -> (CN Char 0 : old) :| rest)) . (mode .~ Insert)

fixCursor :: App -> App
fixCursor app@App {..} = cursors . ix 0 %~ (\c -> coerceCursor _contents c c) $ app

insertAfter :: Prelude.Char -> App -> App
insertAfter c app@App {..} = contents %~ insertAt (getCharacterPos _contents (NE.head _cursors)) c $ fixCursor app

insert :: Prelude.Char -> App -> App
insert char app@App {..} = cursors . ix 0 %~ (\c -> coerceCursor contents' [CN Char (getCharacterPos contents' c + 1)] c) $ contents .~ contents' $ app
  where contents' = insertAt (getCharacterPos _contents (NE.head _cursors)) char _contents

deleteUnderCursor :: App -> App
deleteUnderCursor app@App {..} = contents %~ deleteMany (getCursorRange _contents (NE.head _cursors)) $ app

deleteCharacter :: App -> App
deleteCharacter app@App {..} = cursors . ix 0 %~ (\c -> coerceCursor contents' [CN Char (getCharacterPos contents' c ?- (1 :: Int))] c) $ contents .~ contents' $ app
  where contents' = deleteAt (getCharacterPos _contents (NE.head _cursors) ?- (1 :: Int) :: Int) _contents

yank :: App -> IO App
yank app@App {..} = callProcess "wl-copy" [slice (getCursorRange _contents (NE.head _cursors)) _contents] `catch` (\(_ :: IOError) -> return ()) >> pure app

paste :: App -> IO App
paste app@App {..} = (\clipboard -> contents %~ (insertMany (getCharacterPos _contents (NE.head _cursors)) clipboard . deleteMany (getCursorRange _contents (NE.head _cursors))) $ app) <$> readProcess "wl-paste" [] ""

undo :: App -> App
undo app@App {..} = case _history of
  entry:rest -> contents .~ entry $ history .~ rest $ historyBack %~ (_contents :) $ app
  _ -> app

redo :: App -> App
redo app@App {..} = case _historyBack of
  entry:rest -> contents .~ entry $ historyBack .~ rest $ history %~ (_contents :) $ app
  _ -> app


saveHistory :: App -> App
saveHistory app@App {..} = historyBack .~ [] $ history %~ (_contents :) $ app