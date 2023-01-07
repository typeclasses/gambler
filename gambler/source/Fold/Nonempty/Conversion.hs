-- | Getting a 'NonemptyFold' from some other type of fold
module Fold.Nonempty.Conversion where

import Fold.Nonempty.Type

import Fold.Effectful.Type (EffectfulFold)
import Fold.Pure.Type (Fold (Fold))

import qualified Fold.Pure.Type as Fold
import qualified Fold.Pure.Conversion as Fold.Conversion

import Data.Functor.Identity (Identity)

{-| Turn a regular fold that allows empty input into a fold that
requires at least one input -}
fold :: Fold a b -> NonemptyFold a b
fold Fold{ Fold.step, Fold.initial, Fold.extract } =
    NonemptyFold{ initial = step initial, step, extract }

{-| Turn an effectful fold into a pure fold that requires at least
one input -}
effectfulFold :: EffectfulFold Identity a b -> NonemptyFold a b
effectfulFold x = fold (Fold.Conversion.effectfulFold x)
