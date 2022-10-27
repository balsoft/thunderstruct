module System.Clipboard where

import Data.Functor (void)
import System.Environment (getEnv)
import System.Process

setClipboard :: String -> IO ()
setClipboard s = do
  session <- getEnv "XDG_SESSION_TYPE"
  let command = case session of "wayland" -> ("wl-copy", []); _ -> ("xsel", ["--input", "--clipboard"])
  void $ uncurry readProcess command s

getClipboard :: IO String
getClipboard = do
  session <- getEnv "XDG_SESSION_TYPE"
  let command = case session of "wayland" -> ("wl-paste", ["--no-newline"]); _ -> ("xsel", ["--output", "--clipboard"])
  uncurry readProcess command ""