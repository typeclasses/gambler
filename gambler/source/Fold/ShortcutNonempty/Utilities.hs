module Fold.ShortcutNonempty.Utilities where

import Fold.ShortcutNonempty.Type

import Fold.Shortcut.Type (ShortcutFold (ShortcutFold))
import Strict (willSave)

import qualified Strict
import qualified Fold.Shortcut.Type as Empty

{-| Causes a shortcut fold to stop once it becomes ambivalent -}
demotivate :: ShortcutNonemptyFold a b -> ShortcutNonemptyFold a b
demotivate ShortcutNonemptyFold{ initial, step, extract } =
  ShortcutNonemptyFold
    { initial = \a -> willSave (initial a)
    , step = \x a -> willSave (step x a)
    , extract = \v -> case v of
        Dead e -> case e of
          Strict.Left x -> extract (Dead x)
          Strict.Right x -> extract (Alive Ambivalent x)
        Alive w x -> extract (Alive w x)
    }

{-| Allows to continue feeding a fold even after passing it to a function
that closes it -}
duplicate :: ShortcutNonemptyFold a b -> ShortcutNonemptyFold a (ShortcutFold a b)
duplicate ShortcutNonemptyFold{ initial, step, extract } =
  ShortcutNonemptyFold
    { initial
    , step
    , extract = \v -> ShortcutFold{ Empty.initial = v, Empty.step, Empty.extract }
    }
