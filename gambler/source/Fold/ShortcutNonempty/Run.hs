module Fold.ShortcutNonempty.Run where

import Fold.ShortcutNonempty.Type

import Data.List.NonEmpty (NonEmpty ((:|)))

{-| Fold a nonempty listlike container to a single summary result,
    forcing only enough input to satisfy the short-cutting fold's tenacity -}
run :: ShortcutNonemptyFold a b -> NonEmpty a -> b
run ShortcutNonemptyFold{ initial, step, extract } =
    \(z :| as) -> go (initial z) as
  where
    go (Alive Tenacious x) (a : as) = go (step x a) as
    go v _ = extract v
