module Fold.ShortcutNonempty.Conversion where

import Fold.ShortcutNonempty.Type

import Data.Functor.Identity (Identity)
import Data.Void (Void)
import Fold.Effectful.Type (EffectfulFold)
import Fold.Nonempty.Type (NonemptyFold (NonemptyFold))
import Fold.Pure.Type (Fold (Fold))
import Fold.Shortcut.Type (ShortcutFold (ShortcutFold))

import qualified Fold.Pure.Conversion as Fold
import qualified Fold.Pure.Type as Fold
import qualified Fold.Shortcut.Type as Shortcut
import qualified Fold.Nonempty.Type as Nonempty

fold :: Fold a b -> ShortcutNonemptyFold a b
fold Fold{ Fold.initial = initial :: x, Fold.step, Fold.extract } =
    ShortcutNonemptyFold
      { initial = \a -> Alive Ambivalent (step initial a)
      , step = \x a -> Alive Ambivalent (step x a) :: Vitality Void x
      , extract = \v -> case v of { Alive _ x -> extract x }
      }

effectfulFold :: EffectfulFold Identity a b -> ShortcutNonemptyFold a b
effectfulFold x = fold (Fold.effectfulFold x)

nonemptyFold :: forall a b. NonemptyFold a b -> ShortcutNonemptyFold a b
nonemptyFold
  NonemptyFold{ Nonempty.initial = initial :: a -> x, Nonempty.step, Nonempty.extract } =
    ShortcutNonemptyFold
      { initial = \a -> Alive Ambivalent (initial a)
      , step = \x a -> Alive Ambivalent (step x a) :: Vitality Void x
      , extract = \v -> case v of { Alive _ x -> extract x }
      }

shortcutFold :: ShortcutFold a b -> ShortcutNonemptyFold a b
shortcutFold ShortcutFold{ Shortcut.initial, Shortcut.step, Shortcut.extract } =
    ShortcutNonemptyFold
      { initial = case initial of { Dead x -> \_ -> Dead x; Alive _ x -> step x }
      , step = step
      , extract
      }
