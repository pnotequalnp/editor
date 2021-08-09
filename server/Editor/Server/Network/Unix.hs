module Editor.Server.Network.Unix where

import Data.Store (decodeEx, encode)
import Editor.Server.Network qualified as S
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as N
import UnliftIO (liftIO)

newtype UnixSocket = UnixSocket N.Socket

instance S.Connection UnixSocket where
  receive (UnixSocket sock) = fmap decodeEx . liftIO $ N.recv sock 4096
  send (UnixSocket sock) = liftIO . N.sendAll sock . encode

instance S.Listen UnixSocket where
  type Conn UnixSocket = UnixSocket
  accept (UnixSocket sock) = fmap (UnixSocket . fst) . liftIO $ N.accept sock

init :: FilePath -> IO UnixSocket
init fp = do
  sock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
  N.bind sock $ N.SockAddrUnix fp
  N.listen sock 1
  pure $ UnixSocket sock
