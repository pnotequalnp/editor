module Editor.Client where

import Brick qualified as B
import Editor.Client.Network (Conn (..), Connection (..))
import Editor.Message (ClientMessage (..))
import Graphics.Vty qualified as V

newtype AppState = AppState {stateConn :: Conn}

data ResourceName = ResourceName deriving stock (Eq, Ord)

data Event = Event

main :: Connection conn => conn -> IO ()
main conn = do
  let initialState = AppState {stateConn = Conn conn}
  _finalState <- B.defaultMain app initialState
  pure ()

app :: B.App AppState Event ResourceName
app =
  B.App
    { B.appDraw = draw,
      B.appChooseCursor = chooseCursor,
      B.appHandleEvent = handleEvent,
      B.appStartEvent = startEvent,
      B.appAttrMap = attrMap
    }

draw :: AppState -> [B.Widget ResourceName]
draw _s = [B.emptyWidget]

chooseCursor :: AppState -> [B.CursorLocation ResourceName] -> Maybe (B.CursorLocation ResourceName)
chooseCursor _s _ = Nothing

handleEvent :: AppState -> B.BrickEvent ResourceName Event -> B.EventM ResourceName (B.Next AppState)
handleEvent s (B.VtyEvent (V.EvKey k ms)) = handleKey s k ms
handleEvent s (B.AppEvent _e) = B.continue s
handleEvent s _e = B.continue s

handleKey :: AppState -> V.Key -> [V.Modifier] -> B.EventM ResourceName (B.Next AppState)
handleKey s (V.KChar 'q') _ms = B.halt s
handleKey s (V.KChar 'n') _ms = send (stateConn s) NewBuffer *> B.continue s
handleKey s _k _ms = B.continue s

startEvent :: AppState -> B.EventM ResourceName AppState
startEvent = pure

attrMap :: AppState -> B.AttrMap
attrMap _s = B.attrMap V.defAttr []
