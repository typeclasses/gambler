-- | Folds from "Fold.Pure.ShortcutNonempty" trivially lifted into 'NonemptyFold'
module Fold.Nonempty.ShortcutNonempty where

import Fold.Nonempty.Conversion (shortcutNonemptyFold)
import Fold.Nonempty.Type (NonemptyFold)

import qualified Fold.ShortcutNonempty.Examples as ShortcutNonempty

{-| The first input -}
first :: NonemptyFold a a
first = shortcutNonemptyFold ShortcutNonempty.first
