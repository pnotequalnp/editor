module Editor.Server.Network.Unix where

import Data.Store (decodeEx, encode)
import Editor.Server.Network qualified as S
import Network.Socket qualified as N
import Network.Socket.ByteString qualified as N

newtype UnixSocket = UnixSocket N.Socket

instance S.Connection UnixSocket where
  receive (UnixSocket sock) = decodeEx <$> N.recv sock 4096
  send (UnixSocket sock) msg = N.sendAll sock $ encode msg

instance S.Listen UnixSocket where
  type Conn UnixSocket = UnixSocket
  accept (UnixSocket sock) = UnixSocket . fst <$> N.accept sock

init :: FilePath -> IO UnixSocket
init fp = do
  sock <- N.socket N.AF_UNIX N.Stream N.defaultProtocol
  N.bind sock $ N.SockAddrUnix fp
  N.listen sock 1
  pure $ UnixSocket sock
