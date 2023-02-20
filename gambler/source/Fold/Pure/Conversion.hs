-- | Getting a 'Fold' from some other type of fold
module Fold.Pure.Conversion where

import Fold.Pure.Type

import Data.Function (($))
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity, runIdentity)
import Data.Maybe (Maybe)
import Fold.Effectful.Type (EffectfulFold (EffectfulFold))
import Fold.Nonempty.Type (NonemptyFold (NonemptyFold))
import Fold.Shortcut.Type (ShortcutFold (ShortcutFold))
import Fold.ShortcutNonempty.Type (ShortcutNonemptyFold (ShortcutNonemptyFold))
import Strict (shortcut)

import qualified Fold.Effectful.Type as Effectful
import qualified Fold.Nonempty.Type as Nonempty
import qualified Fold.ShortcutNonempty.Type as ShortcutNonempty
import qualified Fold.Shortcut.Type as Shortcut
import qualified Strict

{-| Turn an effectful fold into a pure fold -}
effectfulFold :: EffectfulFold Identity a b -> Fold a b
effectfulFold
  EffectfulFold{ Effectful.initial, Effectful.step, Effectful.extract } =
    Fold
      { initial =         runIdentity ( initial   )
      , step    = \x a -> runIdentity ( step x a  )
      , extract = \x   -> runIdentity ( extract x )
      }

{-| Turn a fold that requires at least one input into a fold that returns
'Data.Maybe.Nothing' when there are no inputs -}
nonemptyFold :: NonemptyFold a b -> Fold a (Maybe b)
nonemptyFold
  NonemptyFold{ Nonempty.initial, Nonempty.step, Nonempty.extract } =
    Fold
      { initial = Strict.Nothing
      , step = \xm a -> Strict.Just $ case xm of
            Strict.Nothing -> initial a
            Strict.Just x -> step x a
      , extract = \xm -> extract <$> Strict.lazy xm
      }

shortcutFold :: ShortcutFold a b -> Fold a b
shortcutFold ShortcutFold{
        Shortcut.initial, Shortcut.step, Shortcut.extract } =
    Fold
      { initial = shortcut initial
      , step = \x a -> shortcut $ step x a
      , extract = extract
      }

shortcutNonemptyFold :: ShortcutNonemptyFold a b -> Fold a (Maybe b)
shortcutNonemptyFold ShortcutNonemptyFold{
        ShortcutNonempty.initial, ShortcutNonempty.step, ShortcutNonempty.extract } =
    Fold
      { initial = Strict.Nothing
      , step = \xm a -> Strict.Just $ shortcut $ case xm of
            Strict.Nothing -> initial a
            Strict.Just x -> step x a
      , extract = \xm -> extract <$> Strict.lazy xm
      }
