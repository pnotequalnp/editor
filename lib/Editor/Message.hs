module Editor.Message where

import Data.Store ()
import Data.Store.TH (makeStore)

import Editor.Diff (Diff)

data ClientMessage
  = BufferDiff Int Diff
  | BufferUndo Int Int
  | NewBuffer
  | DeleteBuffer Int
  deriving stock (Show)

makeStore ''ClientMessage

newtype ServerMessage = ServerMessage ()
  deriving stock (Show)

makeStore ''ServerMessage
