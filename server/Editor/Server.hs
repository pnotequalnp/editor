module Editor.Server where

import Control.Lens
import Control.Monad (forever)
import Control.Monad.Reader
import Data.IntMap (IntMap)
import Data.IntMap qualified as IM
import Editor.Buffer (Buffer, newBuffer)
import Editor.Message (ClientMessage (..))
import Editor.Server.Network (Connection (..), Listen (..))
import UnliftIO.Async (async)
import UnliftIO.STM

data Env = Env
  { _buffers :: TVar (IntMap Buffer),
    _nextBufferId :: TVar Int
  }

makeLenses ''Env

initEnv :: IO Env
initEnv = Env <$> newTVarIO IM.empty <*> newTVarIO 0

main :: Listen sock => sock -> IO ()
main sock = do
  env <- initEnv
  forever $
    accept sock >>= async . server env

server :: (MonadIO m, Connection conn) => Env -> conn -> m ()
server env conn = flip runReaderT env do
  msg <- receive conn
  liftIO $ putStrLn "Received"
  handler msg
  liftIO $ putStrLn "Handled:"
  view buffers >>= readTVarIO >>= liftIO . print

handler :: (MonadReader Env m, MonadIO m) => ClientMessage -> m ()
handler = \case
  NewBuffer -> do
    Env buffs nextId <- ask
    atomically do
      buffId <- readTVar nextId
      modifyTVar' nextId succ
      modifyTVar' buffs $ at buffId ?~ newBuffer
  _ -> pure ()
