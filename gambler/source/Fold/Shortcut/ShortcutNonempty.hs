-- | Folds from "Fold.ShortcutNonempty.Examples" trivially lifted into 'ShortcutFold'
module Fold.Shortcut.ShortcutNonempty where

import Data.Maybe (Maybe)
import Fold.Shortcut.Conversion (shortcutNonemptyFold)
import Fold.Shortcut.Type (ShortcutFold)

import qualified Fold.ShortcutNonempty.Examples as ShortcutNonempty

{-| The first input -}
first :: ShortcutFold a (Maybe a)
first = shortcutNonemptyFold ShortcutNonempty.first
