module Fold.Shortcut.Run where

import Fold.Shortcut.Type

{-| Fold a listlike container to a single summary result, forcing
    only enough input to satisfy the short-cutting fold -}
run :: ShortcutFold a b -> [a] -> b
run ShortcutFold{ initial, step, extractDead, extractLive } = go initial
  where
    go (Alive Tenacious x) (a : as)  =  go (step x a) as
    go (Alive Tenacious x) []        =  extractLive x
    go (Alive Ambivalent x) _        =  extractLive x
    go (Dead x)             _        =  extractDead x
