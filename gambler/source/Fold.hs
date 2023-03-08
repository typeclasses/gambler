module Fold
  (
    {- * Fold types -} Fold (Fold), NonemptyFold (NonemptyFold),
            EffectfulFold (EffectfulFold), ShortcutFold (ShortcutFold),
            ShortcutNonemptyFold (ShortcutNonemptyFold), Vitality (..), Will (..),
    {- * Running -} runFold, runNonemptyFold, runEffectfulFold,
    {- * Search -} element, notElement, find, lookup,
    {- * Arithmetic folds -} sum, product, mean, variance, standardDeviation,
    {- * Working with indices -} index, findIndex, elementIndex,
    {- * Counting inputs -} null, length,
    {- * Boolean folds -} and, or, all, any,
    {- * Min/max -} maximum, minimum, maximumBy, minimumBy,
    {- * First/last -} first, last,
    {- * General folds -} magma, semigroup, monoid, effect, effectMonoid,
    {- * List folds -} list, reverseList, nonemptyList, reverseNonemptyList,
    {- * Fold conversions -} emptyToNonempty, nonemptyToEmpty, pureToEffectful,
            effectfulToPure, nonemptyToEffectful, effectfulToNonempty,
    {- * Hoist -} hoist,
    {- * Duplicate -} duplicateFold, duplicateNonemptyFold, duplicateEffectfulFold,
  )
  where

import Fold.Effectful.Type
import Fold.Nonempty.Type
import Fold.Pure.Type
import Fold.Shortcut.Type
import Fold.ShortcutNonempty.Type

import Fold.Effectful.Examples.Interesting
import Fold.Nonempty.Examples.Interesting hiding (list, reverseList, sum, product)
import Fold.Pure.Examples.Interesting
import Fold.Shortcut.Examples.Interesting
import Fold.ShortcutNonempty.Examples.Interesting

import Fold.Effectful.Utilities

import qualified Fold.Effectful.Conversion as ConvertTo.Effectful
import qualified Fold.Nonempty.Conversion as ConvertTo.Nonempty
import qualified Fold.Pure.Conversion as ConvertTo.Pure

import qualified Fold.Effectful as Effectful
import qualified Fold.Nonempty as Nonempty
import qualified Fold.Pure as Pure

import Control.Applicative (Applicative)
import Control.Monad (Monad)
import Data.Foldable (Foldable)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe)

{-| Fold a listlike container to a single summary result -}
runFold :: Foldable f => Fold a b -> f a -> b
runFold = Pure.run

{-| Fold a nonempty listlike container to a single summary result -}
runNonemptyFold :: NonemptyFold a b -> NonEmpty a -> b
runNonemptyFold = Nonempty.run

{-| Fold an listlike container to an action that produces a single summary
result -}
runEffectfulFold :: Foldable f => Monad m => EffectfulFold m a b -> f a -> m b
runEffectfulFold = Effectful.run

{-| Turn a regular fold that allows empty input into a fold that
requires at least one input -}
emptyToNonempty :: Fold a b -> NonemptyFold a b
emptyToNonempty = ConvertTo.Nonempty.fold

{-| Turn an effectful fold into a pure fold that requires at least
one input -}
effectfulToNonempty :: EffectfulFold Identity a b -> NonemptyFold a b
effectfulToNonempty = ConvertTo.Nonempty.effectfulFold

{-| Turn a fold that requires at least one input into a fold that returns
    'Data.Maybe.Nothing' when there are no inputs -}
nonemptyToEmpty :: NonemptyFold a b -> Fold a (Maybe b)
nonemptyToEmpty = ConvertTo.Pure.nonemptyFold

{-| Generalize a pure fold to an effectful fold -}
pureToEffectful :: Monad m => Fold a b -> EffectfulFold m a b
pureToEffectful = ConvertTo.Effectful.fold

{-| Turn an effectful fold into a pure fold -}
effectfulToPure :: EffectfulFold Identity a b -> Fold a b
effectfulToPure = ConvertTo.Pure.effectfulFold

{-| Turn a nonempty fold that requires at least one input into a fold that
    returns 'Data.Maybe.Nothing' when there are no inputs -}
nonemptyToEffectful :: Monad m =>
    NonemptyFold a b -> EffectfulFold m a (Maybe b)
nonemptyToEffectful = ConvertTo.Effectful.nonemptyFold

{-| All the inputs from a nonempty fold -}
nonemptyList :: NonemptyFold a (NonEmpty a)
nonemptyList = Nonempty.list

{-| All the inputs from a nonempty fold, in reverse order -}
reverseNonemptyList :: NonemptyFold a (NonEmpty a)
reverseNonemptyList = Nonempty.reverseList

{-| Allows to continue feeding a fold even after passing it to a function
    that closes it -}
duplicateFold :: Fold a b -> Fold a (Fold a b)
duplicateFold = Pure.duplicate

{-| Allows to continue feeding a fold even after passing it to a function
    that closes it -}
duplicateNonemptyFold :: NonemptyFold a b -> NonemptyFold a (Fold a b)
duplicateNonemptyFold = Nonempty.duplicate

{-| Allows to continue feeding an effectful fold even after passing it to a
    function that closes it -}
duplicateEffectfulFold :: Applicative m => EffectfulFold m a b -> EffectfulFold m a (EffectfulFold m a b)
duplicateEffectfulFold = Effectful.duplicate
