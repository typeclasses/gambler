-- | Getting a 'NonemptyFold' from some other type of fold
module Fold.Nonempty.Conversion where

import Fold.Nonempty.Type

import Fold.Effectful.Type (EffectfulFold)
import Fold.Pure.Type (Fold (Fold))
import Fold.ShortcutNonempty.Type (ShortcutNonemptyFold (ShortcutNonemptyFold))
import Strict (shortcut)

import qualified Fold.Pure.Type as Fold
import qualified Fold.Pure.Conversion as Fold.Conversion
import qualified Fold.ShortcutNonempty.Type as Shortcut

import Data.Functor.Identity (Identity)

{-| Turn a regular fold that allows empty input into a fold that
requires at least one input -}
fold :: Fold a b -> NonemptyFold a b
fold Fold{ Fold.initial, Fold.step, Fold.extract } =
    NonemptyFold{ initial = step initial, step, extract }

{-| Turn an effectful fold into a pure fold that requires at least
one input -}
effectfulFold :: EffectfulFold Identity a b -> NonemptyFold a b
effectfulFold x = fold (Fold.Conversion.effectfulFold x)

shortcutNonemptyFold :: ShortcutNonemptyFold a b -> NonemptyFold a b
shortcutNonemptyFold ShortcutNonemptyFold{
        Shortcut.step, Shortcut.initial, Shortcut.extract } =
    NonemptyFold{ initial = \a -> shortcut (initial a),
        step = \x a -> shortcut (step x a), extract }
