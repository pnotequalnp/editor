module Editor.Server.Network where

import Editor.Message (ClientMessage, ServerMessage)

class Connection (Conn sock) => Listen sock where
  type Conn sock
  accept :: sock -> IO (Conn sock)

class Connection conn where
  receive :: conn -> IO ClientMessage
  send :: conn -> ServerMessage -> IO ()
