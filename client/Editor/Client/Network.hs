module Editor.Client.Network where

import Editor.Message (ClientMessage, ServerMessage)
import UnliftIO (MonadIO)

class Connection conn where
  receive :: MonadIO m => conn -> m ServerMessage
  send :: MonadIO m => conn -> ClientMessage -> m ()
