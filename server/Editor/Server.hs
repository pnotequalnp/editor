module Editor.Server where

import Control.Lens
import Control.Monad (forever)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Editor.Buffer (Buffer, newBuffer)
import Editor.Message (ClientMessage (..))
import Editor.Server.Network (Connection (..), Listen (..))
import UnliftIO.Async (async)
import UnliftIO.STM

main :: Listen sock => sock -> IO ()
main sock = do
  buffers <- newTVarIO IM.empty
  nextBuffId <- newTVarIO (0 :: Int)
  forever do
    conn <- accept sock
    async do
      msg <- receive conn
      putStrLn "Received"
      handler buffers nextBuffId msg
      putStrLn "Handled:"
      readTVarIO buffers >>= print

handler :: TVar (IntMap Buffer) -> TVar Int -> ClientMessage -> IO ()
handler buffers nextBuffId = \case
  NewBuffer -> atomically do
    buffId <- readTVar nextBuffId
    writeTVar nextBuffId $ buffId + 1
    modifyTVar' buffers $ at buffId ?~ newBuffer
  _ -> pure ()
