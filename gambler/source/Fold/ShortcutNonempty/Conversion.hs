module Fold.ShortcutNonempty.Conversion where

import Fold.ShortcutNonempty.Type

import Data.Functor.Identity (Identity)
import Fold.Effectful.Type (EffectfulFold)
import Fold.Nonempty.Type (NonemptyFold)
import Fold.Pure.Type (Fold (Fold))
import Fold.Shortcut.Type (ShortcutFold (ShortcutFold))
import Fold.Nonempty.Type (NonemptyFold (NonemptyFold))
import Data.Void (absurd)

import qualified Fold.Pure.Conversion as Fold
import qualified Fold.Pure.Type as Fold
import qualified Fold.Shortcut.Type as Shortcut
import qualified Fold.Nonempty.Type as Nonempty

fold :: Fold a b -> ShortcutNonemptyFold a b
fold
  Fold{ Fold.initial, Fold.step, Fold.extract } =
    ShortcutNonemptyFold
      { initial = \a -> Alive Ambivalent (step initial a)
      , step = \x a -> Alive Ambivalent (step x a)
      , extract = \v -> case v of { Alive _ x -> extract x; Dead x -> absurd x }
      }

effectfulFold :: EffectfulFold Identity a b -> ShortcutNonemptyFold a b
effectfulFold x = fold (Fold.effectfulFold x)

nonemptyFold :: NonemptyFold a b -> ShortcutNonemptyFold a b
nonemptyFold
  NonemptyFold{ Nonempty.initial, Nonempty.step, Nonempty.extract } =
    ShortcutNonemptyFold
      { initial = \a -> Alive Ambivalent (initial a)
      , step = \x a -> Alive Ambivalent (step x a)
      , extract = \v -> case v of { Alive _ x -> extract x; Dead x -> absurd x }
      }

shortcutFold :: ShortcutFold a b -> ShortcutNonemptyFold a b
shortcutFold ShortcutFold{ Shortcut.initial, Shortcut.step,
        Shortcut.extract } =
    ShortcutNonemptyFold
      { initial = case initial of
            Dead    x -> \_ -> Dead x
            Alive _ x -> step x
      , step = step
      , extract
      }
