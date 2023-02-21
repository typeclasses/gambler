-- | Getting an 'EffectfulFold' from some other type of fold
module Fold.Effectful.Conversion where

import Fold.Effectful.Type

import Control.Monad (Monad)
import Data.Maybe (Maybe)
import Fold.Nonempty.Type (NonemptyFold)
import Fold.Shortcut.Type (ShortcutFold)
import Fold.ShortcutNonempty.Type (ShortcutNonemptyFold)

import qualified Control.Applicative as Applicative
import qualified Fold.Pure.Conversion as Pure
import qualified Fold.Pure.Type as Pure

{-| Generalize a pure fold to an effectful fold -}
fold :: Monad m => Pure.Fold a b -> EffectfulFold m a b
fold Pure.Fold{ Pure.initial, Pure.step, Pure.extract } = EffectfulFold
    { initial =         Applicative.pure ( initial   )
    , step    = \x a -> Applicative.pure ( step x a  )
    , extract = \x   -> Applicative.pure ( extract x )
    }

{-| Turn a nonempty fold that requires at least one input into a fold that
returns 'Data.Maybe.Nothing' when there are no inputs -}
nonemptyFold :: Monad m => NonemptyFold a b -> EffectfulFold m a (Maybe b)
nonemptyFold x = fold (Pure.nonemptyFold x)

shortcutFold :: Monad m =>  ShortcutFold a b -> EffectfulFold m a b
shortcutFold x = fold (Pure.shortcutFold x)

shortcutNonemptyFold :: Monad m => ShortcutNonemptyFold a b -> EffectfulFold m a (Maybe b)
shortcutNonemptyFold x = fold (Pure.shortcutNonemptyFold x)
