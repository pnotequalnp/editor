module Editor.Client.Network.Unix where

import Data.Store (decodeEx, encode)
import Editor.Client.Network qualified as C
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as N
import UnliftIO (liftIO)

newtype UnixSocket = UnixSocket N.Socket

instance C.Connection UnixSocket where
  receive (UnixSocket sock) = liftIO . fmap decodeEx $ N.recv sock 4096
  send (UnixSocket sock) msg = liftIO . N.sendAll sock $ encode msg

init :: FilePath -> IO UnixSocket
init fp = do
  sock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
  N.connect sock $ N.SockAddrUnix fp
  pure $ UnixSocket sock
