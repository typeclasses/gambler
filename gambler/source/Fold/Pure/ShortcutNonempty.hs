-- | Folds from "Fold.ShortcutNonempty.Examples" trivially lifted into 'Fold'
module Fold.Pure.ShortcutNonempty where

import Data.Maybe (Maybe)
import Fold.Pure.Conversion (shortcutNonemptyFold)
import Fold.Pure.Type (Fold)

import qualified Fold.ShortcutNonempty.Examples as ShortcutNonempty

{-| The first input -}
first :: Fold a (Maybe a)
first = shortcutNonemptyFold ShortcutNonempty.first
