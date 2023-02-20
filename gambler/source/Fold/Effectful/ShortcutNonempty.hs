-- | Folds from "Fold.Pure.ShortcutNonempty" trivially lifted into 'EffectfulFold'
module Fold.Effectful.ShortcutNonempty where

import Control.Monad (Monad)
import Data.Maybe (Maybe)
import Fold.Effectful.Conversion (fold)
import Fold.Effectful.Type (EffectfulFold)

import qualified Fold.Pure.ShortcutNonempty as Pure

{-| The first input -}
first :: Monad m => EffectfulFold m a (Maybe a)
first = fold Pure.first
