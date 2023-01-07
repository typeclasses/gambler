module Fold.Nonempty.Run where

import Fold.Nonempty.Type

import Data.List.NonEmpty (NonEmpty ((:|)))
import Prelude (($!))

import qualified Data.Foldable as F

{-| Fold a nonempty listlike container to a single summary result -}
run :: NonemptyFold a b -> NonEmpty a -> b
run NonemptyFold{ initial, step, extract } (z :| as) =
    F.foldr cons extract as (initial z)
  where
    cons a k x = k $! step x a
