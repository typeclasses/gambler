-- | Folds of other types trivially lifted into 'Shortcut'
module Fold.Shortcut.Examples.Boring
  (
    {- * Arithmetic folds -} sum, product, mean, variance, standardDeviation,
    {- * Counting inputs -} length,
    {- * Min/max -} maximum, minimum, maximumBy, minimumBy,
    {- * First/last -} first, last,
    {- * General folds -} magma, semigroup, monoid,
    {- * List folds -} list, reverseList,
  )
  where

import Data.Maybe (Maybe)
import Fold.Shortcut.Type (ShortcutFold)
import Data.Semigroup (Semigroup)
import Data.Ord (Ord, Ordering (GT), max, min)
import Data.Monoid (Monoid)
import Prelude (Floating, Fractional, Num)
import Numeric.Natural (Natural)

import qualified Fold.ShortcutNonempty.Examples.Interesting as ShortcutNonempty
import qualified Fold.Nonempty.Examples.Interesting as Nonempty
import qualified Fold.Pure.Examples.Interesting as Fold
import qualified Fold.Shortcut.Conversion as Convert

{-| The first input -}
first :: ShortcutFold a (Maybe a)
first = Convert.shortcutNonemptyFold ShortcutNonempty.first

{-| Start with the first input, append each new input on the right
with the given function -}
magma :: (a -> a -> a) -> ShortcutFold a (Maybe a)
magma step = Convert.nonemptyFold (Nonempty.magma step)

{-| Append each new input on the right with ('<>') -}
semigroup :: Semigroup a => ShortcutFold a (Maybe a)
semigroup = Convert.nonemptyFold Nonempty.semigroup

{-| The last input -}
last :: ShortcutFold a (Maybe a)
last = Convert.nonemptyFold Nonempty.last

{-| The greatest input -}
maximum :: Ord a => ShortcutFold a (Maybe a)
maximum = magma max

{-| The greatest input with respect to the given comparison function -}
maximumBy :: (a -> a -> Ordering) -> ShortcutFold a (Maybe a)
maximumBy cmp = magma (\x y -> case cmp x y of { GT -> x; _ -> y })

{-| The least input -}
minimum :: Ord a => ShortcutFold a (Maybe a)
minimum = magma min

{-| The least input with respect to the given comparison function -}
minimumBy :: (a -> a -> Ordering) -> ShortcutFold a (Maybe a)
minimumBy cmp = magma (\x y -> case cmp x y of { GT -> y; _ -> x })

{-| All the inputs -}
list :: ShortcutFold a [a]
list = Convert.fold Fold.list

{-| All the inputs in reverse order -}
reverseList :: ShortcutFold a [a]
reverseList = Convert.fold Fold.reverseList

{-| Start with 'mempty', append each input on the right with ('<>') -}
monoid :: Monoid a => ShortcutFold a a
monoid = Convert.fold Fold.monoid

{-| The number of inputs -}
length :: ShortcutFold a Natural
length = Convert.fold Fold.length

{-| Adds the inputs -}
sum :: Num a => ShortcutFold a a
sum = Convert.fold Fold.sum

{-| Multiplies the inputs -}
product :: Num a => ShortcutFold a a
product = Convert.fold Fold.product

{-| Numerically stable arithmetic mean of the inputs -}
mean :: Fractional a => ShortcutFold a a
mean = Convert.fold Fold.mean

{-| Numerically stable (population) variance over the inputs -}
variance :: Fractional a => ShortcutFold a a
variance = Convert.fold Fold.variance

{-| Numerically stable (population) standard deviation over the inputs -}
standardDeviation :: Floating a => ShortcutFold a a
standardDeviation = Convert.fold Fold.standardDeviation
