module Editor.Client.Network where

import Editor.Message (ClientMessage, ServerMessage)
import UnliftIO (MonadIO)

data Conn = forall conn. Connection conn => Conn conn

instance Connection Conn where
  receive (Conn conn) = receive conn
  send (Conn conn) = send conn

class Connection conn where
  receive :: MonadIO m => conn -> m ServerMessage
  send :: MonadIO m => conn -> ClientMessage -> m ()
