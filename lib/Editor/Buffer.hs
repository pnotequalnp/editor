module Editor.Buffer where

import Control.Lens
import Data.Text (Text)
import Data.Vector (Vector)
import Editor.Diff

data Buffer = Buffer
  { _buffHistory :: [Diff],
    _buffPath :: Maybe FilePath,
    _buffRoot :: Vector Text
  }
  deriving stock (Show)

makeLenses ''Buffer

newBuffer :: Buffer
newBuffer = Buffer [] Nothing mempty
