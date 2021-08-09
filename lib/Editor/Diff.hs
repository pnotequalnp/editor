module Editor.Diff where

import Control.Lens
import Data.Group (Group (..))
import Data.Store ()
import Data.Store.TH (makeStore)
import Data.Text (Text)

data Operation
  = InsertLine Int Text
  | DeleteLine Int Text
  | ReplaceLine Int Text Text
  deriving stock (Show)

makeStore ''Operation

newtype Diff = Diff
  { getDiff :: [Operation]
  }
  deriving stock (Show)
  deriving newtype (Monoid, Semigroup)

makeStore ''Diff

instance Group Diff where
  invert = over coerced $ reverse . fmap undo

undo :: Operation -> Operation
undo = \case
  InsertLine n t -> DeleteLine n t
  DeleteLine n t -> InsertLine n t
  ReplaceLine n s t -> ReplaceLine n t s
