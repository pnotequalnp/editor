module Editor.Client where

import Brick qualified as B
import Brick.BChan qualified as B
import Control.Lens
import Control.Monad (forever)
import Data.Text (Text)
import Editor.Client.Network (Conn (..), Connection (..))
import Editor.Message (ClientMessage (..), ServerMessage (ServerMessage))
import Graphics.Vty qualified as V
import UnliftIO.Async (withAsync)

data AppState = AppState {_stateConn :: Conn, _stateMessages :: [Text]}

makeLenses ''AppState

data ResourceName = ResourceName deriving stock (Eq, Ord)

main :: Connection conn => conn -> IO ()
main conn = do
  chan <- B.newBChan 1000
  let initialState = AppState {_stateConn = Conn conn, _stateMessages = ["Started"]}
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  _finalState <- withAsync (forever $ receive conn >>= B.writeBChan chan) \_watcherThread ->
    B.customMain
      initialVty
      buildVty
      (Just chan)
      app
      initialState
  pure ()

app :: B.App AppState ServerMessage ResourceName
app =
  B.App
    { B.appDraw = draw,
      B.appChooseCursor = chooseCursor,
      B.appHandleEvent = handleEvent,
      B.appStartEvent = startEvent,
      B.appAttrMap = attrMap
    }

draw :: AppState -> [B.Widget ResourceName]
draw s = [B.vBox $ B.txt <$> s ^. stateMessages]

chooseCursor :: AppState -> [B.CursorLocation ResourceName] -> Maybe (B.CursorLocation ResourceName)
chooseCursor _s _ = Nothing

handleEvent :: AppState -> B.BrickEvent ResourceName ServerMessage -> B.EventM ResourceName (B.Next AppState)
handleEvent s (B.VtyEvent (V.EvKey k ms)) = handleKey s k ms
handleEvent s (B.AppEvent e) = handleServer s e
handleEvent s _e = B.continue s

handleServer :: AppState -> ServerMessage -> B.EventM ResourceName (B.Next AppState)
handleServer s (ServerMessage ()) = do
  B.continue $ s & stateMessages %~ ("Received" :)

handleKey :: AppState -> V.Key -> [V.Modifier] -> B.EventM ResourceName (B.Next AppState)
handleKey s (V.KChar 'q') _ms = B.halt s
handleKey s (V.KChar 'n') _ms = send (s ^. stateConn) NewBuffer *> B.continue s
handleKey s _k _ms = B.continue s

startEvent :: AppState -> B.EventM ResourceName AppState
startEvent = pure

attrMap :: AppState -> B.AttrMap
attrMap _s = B.attrMap V.defAttr []
