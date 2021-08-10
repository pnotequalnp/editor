module Editor.Client where

import Brick qualified as B
import Editor.Client.Network (Connection (..))
import Editor.Message (ClientMessage (..))
import Graphics.Vty qualified as V

data AppState = AppState
data ResourceName = ResourceName deriving stock (Eq, Ord)
data Event = Event

main :: Connection conn => conn -> IO ()
main conn = do
  send conn NewBuffer
  let initialState = AppState
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

handleEvent :: AppState -> B.BrickEvent ResourceName e -> B.EventM ResourceName (B.Next AppState)
handleEvent s (B.VtyEvent (V.EvKey (V.KChar 'q') [])) = B.halt s
handleEvent s _ = B.continue s

startEvent :: AppState -> B.EventM ResourceName AppState
startEvent = pure

attrMap :: AppState -> B.AttrMap
attrMap _s = B.attrMap V.defAttr []
