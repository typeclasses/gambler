module Fold.ShortcutNonempty.Examples.Interesting
  (
    {- * First/last -} first,
  )
  where

import Fold.ShortcutNonempty.Type

import Data.Void (absurd)

{-| The first input (tenacious) -}
first :: ShortcutNonemptyFold a a
first = ShortcutNonemptyFold
  { initial = Dead
  , step = absurd
  , extract = \v -> case v of { Dead x -> x }
  }
