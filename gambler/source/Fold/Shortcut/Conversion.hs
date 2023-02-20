module Fold.Shortcut.Conversion where

import Fold.Shortcut.Type

import Data.Functor ((<$>))
import Data.Functor.Identity (Identity)
import Data.Maybe (Maybe)
import Fold.Effectful.Type (EffectfulFold)
import Fold.Nonempty.Type (NonemptyFold)
import Fold.Pure.Type (Fold (Fold))
import Fold.ShortcutNonempty.Type (ShortcutNonemptyFold (ShortcutNonemptyFold))

import qualified Fold.Pure.Conversion as Fold
import qualified Fold.Pure.Type as Fold
import qualified Fold.ShortcutNonempty.Type as ShortcutNonempty
import qualified Strict

fold :: Fold a b -> ShortcutFold a b
fold
  Fold{ Fold.initial, Fold.step, Fold.extract } =
    ShortcutFold
      { initial = Shortcut Ambivalent initial
      , step = \x a -> Shortcut Ambivalent (step x a)
      , extract
      }

effectfulFold :: EffectfulFold Identity a b -> ShortcutFold a b
effectfulFold x = fold (Fold.effectfulFold x)

nonemptyFold :: NonemptyFold a b -> ShortcutFold a (Maybe b)
nonemptyFold x = fold (Fold.nonemptyFold x)

shortcutNonemptyFold :: ShortcutNonemptyFold a b -> ShortcutFold a (Maybe b)
shortcutNonemptyFold ShortcutNonemptyFold{
        ShortcutNonempty.initial, ShortcutNonempty.step, ShortcutNonempty.extract } =
    ShortcutFold
      { initial = Shortcut Alive Strict.Nothing
      , step = \xm a -> Strict.Just <$> case xm of
            Strict.Nothing -> initial a
            Strict.Just x -> step x a
      , extract = \xm -> extract <$> Strict.lazy xm
      }
