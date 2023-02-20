module Fold.Shortcut.Run where

import Fold.Shortcut.Type

{-| Fold a listlike container to a single summary result, forcing
    only enough input to satisfy the short-cutting fold -}
run :: ShortcutFold a b -> [a] -> b
run ShortcutFold{ initial, step, extract } = go initial
  where
    go (Shortcut Alive x) (a : as) = go (step x a) as
    go (Shortcut _ x) _ = extract x
