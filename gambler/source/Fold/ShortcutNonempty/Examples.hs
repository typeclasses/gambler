module Fold.ShortcutNonempty.Examples where

import Fold.ShortcutNonempty.Type

import Data.Function (id)
import Prelude (undefined)

{-| The first input -}
first :: ShortcutNonemptyFold a a
first = ShortcutNonemptyFold{ initial = Shortcut Dead, step = undefined, extract = id }
