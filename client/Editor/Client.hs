module Editor.Client where

import Editor.Message (ClientMessage(..))
import Editor.Client.Network (Connection(..))

main :: Connection conn => conn -> IO ()
main conn = do
  send conn NewBuffer
  pure ()
