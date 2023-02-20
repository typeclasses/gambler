-- | Folds from "Fold.Pure.Nonempty" trivially lifted into 'EffectfulFold'
module Fold.Effectful.Nonempty
  (
    {- * General -} magma, semigroup,
    {- * Endpoints -} last,
    {- * Extrema -} maximum, minimum, maximumBy, minimumBy,
  )
  where

import Control.Monad (Monad)
import Data.Maybe (Maybe)
import Data.Ord (Ord, Ordering)
import Data.Semigroup (Semigroup)
import Fold.Effectful.Conversion (fold)
import Fold.Effectful.Type (EffectfulFold)

import qualified Fold.Pure.Nonempty as Pure

{-| Start with the first input, append each new input on the right
with the given function -}
magma :: (a -> a -> a) -> Monad m => EffectfulFold m a (Maybe a)
magma step = fold (Pure.magma step)

{-| Append each new input on the right with ('<>') -}
semigroup :: Semigroup a => Monad m => EffectfulFold m a (Maybe a)
semigroup = fold Pure.semigroup

{-| The last input -}
last :: Monad m => EffectfulFold m a (Maybe a)
last = fold Pure.last

{-| The greatest input -}
maximum :: Ord a => Monad m => EffectfulFold m a (Maybe a)
maximum = fold Pure.maximum

{-| The greatest input with respect to the given comparison function -}
maximumBy :: (a -> a -> Ordering) -> Monad m => EffectfulFold m a (Maybe a)
maximumBy cmp = fold (Pure.maximumBy cmp)

{-| The least input -}
minimum :: Ord a => Monad m => EffectfulFold m a (Maybe a)
minimum = fold Pure.minimum

{-| The least input with respect to the given comparison function -}
minimumBy :: (a -> a -> Ordering) -> Monad m => EffectfulFold m a (Maybe a)
minimumBy cmp = fold (Pure.minimumBy cmp)
