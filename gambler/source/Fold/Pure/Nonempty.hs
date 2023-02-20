-- | Folds from "Fold.Nonempty.Examples" trivially lifted into 'Fold'
module Fold.Pure.Nonempty
  (
    {- * General -} magma, semigroup,
    {- * Endpoints -} last,
    {- * Extrema -} maximum, minimum, maximumBy, minimumBy,
  )
  where

import Data.Maybe (Maybe)
import Data.Ord (Ord, Ordering)
import Data.Semigroup (Semigroup)
import Fold.Pure.Conversion (nonemptyFold)
import Fold.Pure.Type (Fold)

import qualified Fold.Nonempty.Examples as Nonempty

{-| Start with the first input, append each new input on the right
with the given function -}
magma :: (a -> a -> a) -> Fold a (Maybe a)
magma step = nonemptyFold (Nonempty.magma step)

{-| Append each new input on the right with ('<>') -}
semigroup :: Semigroup a => Fold a (Maybe a)
semigroup = nonemptyFold Nonempty.semigroup

{-| The last input -}
last :: Fold a (Maybe a)
last = nonemptyFold Nonempty.last

{-| The greatest input -}
maximum :: Ord a => Fold a (Maybe a)
maximum = nonemptyFold Nonempty.maximum

{-| The greatest input with respect to the given comparison function -}
maximumBy :: (a -> a -> Ordering) -> Fold a (Maybe a)
maximumBy cmp = nonemptyFold (Nonempty.maximumBy cmp)

{-| The least input -}
minimum :: Ord a => Fold a (Maybe a)
minimum = nonemptyFold Nonempty.minimum

{-| The least input with respect to the given comparison function -}
minimumBy :: (a -> a -> Ordering) -> Fold a (Maybe a)
minimumBy cmp = nonemptyFold (Nonempty.minimumBy cmp)
