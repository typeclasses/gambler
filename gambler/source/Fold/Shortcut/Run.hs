module Fold.Shortcut.Run where

import Fold.Shortcut.Type

{-| Fold a listlike container to a single summary result, forcing
    only enough input to satisfy the short-cutting fold's tenacity -}
run :: ShortcutFold a b -> [a] -> b
run ShortcutFold{ initial, step, extract } = go initial
  where
    go (Alive Tenacious x) (a : as) = go (step x a) as
    go v _ = extract v
