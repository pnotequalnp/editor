module Editor.Server.Network.Channel where

import Editor.Message (ClientMessage, ServerMessage)
import Editor.Server.Network (Connection (..))
import UnliftIO.Chan (Chan, readChan, writeChan)

data Channel = Channel
  { toClient :: Chan ServerMessage,
    fromClient :: Chan ClientMessage
  }

instance Connection Channel where
  receive Channel {fromClient} = readChan fromClient
  send Channel {toClient} = writeChan toClient
