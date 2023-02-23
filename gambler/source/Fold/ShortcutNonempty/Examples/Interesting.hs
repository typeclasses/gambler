module Fold.ShortcutNonempty.Examples.Interesting
  (
    {- * First/last -} first,
  )
  where

import Fold.ShortcutNonempty.Type

import Data.Function (id)
import Data.Void (absurd)

{-| The first input (tenacious) -}
first :: ShortcutNonemptyFold a a
first = ShortcutNonemptyFold
  { initial = Dead
  , step = absurd
  , extractDead = id
  , extractLive = absurd
  }
