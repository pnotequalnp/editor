module Editor.Server.Network where

import Editor.Message (ClientMessage, ServerMessage)
import UnliftIO (MonadIO)

class Connection (Conn sock) => Listen sock where
  type Conn sock
  accept :: MonadIO m => sock -> m (Conn sock)

class Connection conn where
  receive :: MonadIO m => conn -> m ClientMessage
  send :: MonadIO m => conn -> ServerMessage -> m ()
