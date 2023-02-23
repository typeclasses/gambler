-- | Folds of other types trivially lifted into 'ShortcutNonempty'
module Fold.ShortcutNonempty.Examples.Boring
  (
    {- * Search -} element, notElement, find, lookup,
    {- * Arithmetic folds -} sum, product, mean, variance, standardDeviation,
    {- * Working with indices -} index, findIndex, elementIndex,
    {- * Counting inputs -} length,
    {- * Boolean folds -} and, or, all, any,
    {- * Min/max -} maximum, minimum, maximumBy, minimumBy,
    {- * First/last -} last,
    {- * General folds -} magma, semigroup, monoid,
    {- * List folds -} list, reverseList,
  )
  where

import Data.Maybe (Maybe)
import Fold.ShortcutNonempty.Type (ShortcutNonemptyFold)
import Data.Semigroup (Semigroup)
import Data.Ord (Ord, Ordering)
import Data.Monoid (Monoid)
import Prelude (Floating, Fractional, Num)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Numeric.Natural (Natural)

import qualified Fold.Shortcut.Examples.Interesting as Shortcut
import qualified Fold.Nonempty.Examples.Interesting as Nonempty
import qualified Fold.Pure.Examples.Interesting as Pure
import qualified Fold.ShortcutNonempty.Conversion as Convert

{-| Start with 'mempty', append each input on the right
    with '<>' (ambivalent) -}
monoid :: Monoid a => ShortcutNonemptyFold a a
monoid = Convert.fold Pure.monoid

{-| The number of inputs (ambivalent) -}
length :: ShortcutNonemptyFold a Natural
length = Convert.fold Pure.length

{-| 'True' if all inputs are 'True' (tenacious) -}
and :: ShortcutNonemptyFold Bool Bool
and = Convert.shortcutFold Shortcut.and

{-| 'True' if any input is 'True' (tenacious) -}
or :: ShortcutNonemptyFold Bool Bool
or = Convert.shortcutFold Shortcut.or

{-| 'True' if all inputs satisfy the predicate (tenacious) -}
all :: (a -> Bool) -> ShortcutNonemptyFold a Bool
all predicate = Convert.shortcutFold (Shortcut.all predicate)

{-| 'True' if any input satisfies the predicate (tenacious) -}
any :: (a -> Bool) -> ShortcutNonemptyFold a Bool
any predicate = Convert.shortcutFold (Shortcut.any predicate)

{-| Adds the inputs (ambivalent) -}
sum :: Num a => ShortcutNonemptyFold a a
sum = Convert.nonemptyFold Nonempty.sum

{-| Multiplies the inputs (ambivalent) -}
product :: Num a => ShortcutNonemptyFold a a
product = Convert.nonemptyFold Nonempty.product

{-| Numerically stable arithmetic mean of the inputs (ambivalent) -}
mean :: Fractional a => ShortcutNonemptyFold a a
mean = Convert.fold Pure.mean

{-| Numerically stable (population) variance over the
    inputs (ambivalent) -}
variance :: Fractional a => ShortcutNonemptyFold a a
variance = Convert.fold Pure.variance

{-| Numerically stable (population) standard deviation over
    the inputs (ambivalent) -}
standardDeviation :: Floating a => ShortcutNonemptyFold a a
standardDeviation = Convert.fold Pure.standardDeviation

{-| 'True' if any input is equal to the given value (tenacious) -}
element :: Eq a => a -> ShortcutNonemptyFold a Bool
element a = Convert.shortcutFold (Shortcut.element a)

{-| 'False' if any input is equal to the given value (tenacious) -}
notElement :: Eq a => a -> ShortcutNonemptyFold a Bool
notElement a = Convert.shortcutFold (Shortcut.notElement a)

{-| The first input that satisfies the predicate, if any (tenacious) -}
find :: (a -> Bool) -> ShortcutNonemptyFold a (Maybe a)
find ok = Convert.shortcutFold (Shortcut.find ok)

{-| The /n/th input, where n=0 is the first input, if the index
    is in bounds (tenacious) -}
index :: Natural -> ShortcutNonemptyFold a (Maybe a)
index i = Convert.shortcutFold (Shortcut.index i)

{-| The index of the first input that matches the given value,
    if any (tenacious) -}
elementIndex :: Eq a => a -> ShortcutNonemptyFold a (Maybe Natural)
elementIndex a = Convert.shortcutFold (Shortcut.elementIndex a)

{-| The index of the first input that satisfies the predicate,
    if any (tenacious) -}
findIndex :: (a -> Bool) -> ShortcutNonemptyFold a (Maybe Natural)
findIndex ok = Convert.shortcutFold (Shortcut.findIndex ok)

{-| The @b@ from the first tuple where @a@ equals the given value,
    if any (tenacious) -}
lookup :: Eq a => a -> ShortcutNonemptyFold (a, b) (Maybe b)
lookup a = Convert.shortcutFold (Shortcut.lookup a)

{-| All the inputs (ambivalent) -}
list :: ShortcutNonemptyFold a [a]
list = Convert.fold Pure.list

{-| All the inputs in reverse order (ambivalent) -}
reverseList :: ShortcutNonemptyFold a [a]
reverseList = Convert.fold Pure.reverseList

{-| Start with the first input, append each new input on the right
    with the given function (ambivalent) -}
magma :: (a -> a -> a) -> ShortcutNonemptyFold a a
magma step = Convert.nonemptyFold (Nonempty.magma step)

{-| Append each new input on the right with '<>' (ambivalent) -}
semigroup :: Semigroup a => ShortcutNonemptyFold a a
semigroup = Convert.nonemptyFold Nonempty.semigroup

{-| The last input  (ambivalent) -}
last :: ShortcutNonemptyFold a a
last = Convert.nonemptyFold Nonempty.last

{-| The greatest input (ambivalent) -}
maximum :: Ord a => ShortcutNonemptyFold a a
maximum = Convert.nonemptyFold Nonempty.maximum

{-| The greatest input with respect to the given comparison
    function (ambivalent) -}
maximumBy :: (a -> a -> Ordering) -> ShortcutNonemptyFold a a
maximumBy cmp = Convert.nonemptyFold (Nonempty.maximumBy cmp)

{-| The least input (ambivalent) -}
minimum :: Ord a => ShortcutNonemptyFold a a
minimum = Convert.nonemptyFold Nonempty.minimum

{-| The least input with respect to the given comparison
    function (ambivalent) -}
minimumBy :: (a -> a -> Ordering) -> ShortcutNonemptyFold a a
minimumBy cmp = Convert.nonemptyFold (Nonempty.minimumBy cmp)
