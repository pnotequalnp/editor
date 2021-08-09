module Editor.Client.Network.Channel where

import Editor.Client.Network
import Editor.Message (ClientMessage, ServerMessage)
import UnliftIO.Chan (Chan, readChan, writeChan)

data Channel = Channel
  { fromServer :: Chan ServerMessage,
    toServer :: Chan ClientMessage
  }

instance Connection Channel where
  receive Channel {fromServer} = readChan fromServer
  send Channel {toServer} = writeChan toServer
