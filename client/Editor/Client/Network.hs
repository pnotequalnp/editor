module Editor.Client.Network where

import Editor.Message (ClientMessage, ServerMessage)

class Connection conn where
  receive :: conn -> IO ServerMessage
  send :: conn -> ClientMessage -> IO ()
