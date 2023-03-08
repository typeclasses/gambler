module Fold.Shortcut.Utilities where

import Fold.Shortcut.Type

import Strict (willSave)

import qualified Strict

{-| Causes a shortcut fold to stop once it becomes ambivalent -}
demotivate :: ShortcutFold a b -> ShortcutFold a b
demotivate ShortcutFold{ initial, step, extract } =
  ShortcutFold
    { initial = willSave initial
    , step = \x a -> willSave (step x a)
    , extract = \v -> case v of
        Dead e -> case e of
          Strict.Left x -> extract (Dead x)
          Strict.Right x -> extract (Alive Ambivalent x)
        Alive w x -> extract (Alive w x)
    }

{-| Allows to continue feeding a fold even after passing it to a function
that closes it -}
duplicate :: ShortcutFold a b -> ShortcutFold a (ShortcutFold a b)
duplicate ShortcutFold{ initial, step, extract } =
  ShortcutFold
    { initial
    , step
    , extract = \v -> ShortcutFold{ initial = v, step, extract }
    }
