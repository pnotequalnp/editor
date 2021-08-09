module Editor.Network where

import Data.Maybe (fromMaybe)
import System.Info (os)
import UnliftIO.Environment (lookupEnv)

defaultPath :: FilePath
defaultPath = case os of
  _ -> "/tmp/editor"

getDefaultPath :: IO FilePath
getDefaultPath = fromMaybe defaultPath <$> lookupEnv "EDITOR_SOCKET"
