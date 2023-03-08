-- | Folds of other types trivially lifted into 'EffectfulFold'
module Fold.Effectful.Examples.Boring
  (
    {- * Monoid -} monoid,
    {- * Length -} null, length,
    {- * Boolean -} and, or, all, any,
    {- * Numeric -} sum, product, mean, variance, standardDeviation,
    {- * Search -} element, notElement, find, lookup,
    {- * Index -} index, findIndex, elementIndex,
    {- * List -} list, reverseList,
    {- * General -} magma, semigroup,
    {- * Endpoints -} first, last,
    {- * Extrema -} maximum, minimum, maximumBy, minimumBy,
  )
  where

import Control.Monad (Monad)
import Data.Maybe (Maybe)
import Data.Ord (Ord, Ordering)
import Data.Semigroup (Semigroup)
import Fold.Effectful.Type (EffectfulFold)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Monoid (Monoid)
import Numeric.Natural (Natural)
import Prelude (Floating, Fractional, Num)

import qualified Fold.Pure.Examples.Interesting as Pure
import qualified Fold.Nonempty.Examples.Interesting as Nonempty
import qualified Fold.Shortcut.Examples.Interesting as Shortcut
import qualified Fold.ShortcutNonempty.Examples.Interesting as ShortcutNonempty
import qualified Fold.Effectful.Conversion as Convert

{-| Start with the first input, append each new input on the right
    with the given function -}
magma :: (a -> a -> a) -> Monad m => EffectfulFold m a (Maybe a)
magma step = Convert.nonemptyFold (Nonempty.magma step)

{-| Append each new input on the right with ('<>') -}
semigroup :: Semigroup a => Monad m => EffectfulFold m a (Maybe a)
semigroup = Convert.nonemptyFold Nonempty.semigroup

{-| The first input -}
first :: Monad m => EffectfulFold m a (Maybe a)
first = Convert.shortcutNonemptyFold ShortcutNonempty.first

{-| The last input -}
last :: Monad m => EffectfulFold m a (Maybe a)
last = Convert.nonemptyFold Nonempty.last

{-| The greatest input -}
maximum :: Ord a => Monad m => EffectfulFold m a (Maybe a)
maximum = Convert.nonemptyFold Nonempty.maximum

{-| The greatest input with respect to the given comparison function -}
maximumBy :: (a -> a -> Ordering) -> Monad m => EffectfulFold m a (Maybe a)
maximumBy cmp = Convert.nonemptyFold (Nonempty.maximumBy cmp)

{-| The least input -}
minimum :: Ord a => Monad m => EffectfulFold m a (Maybe a)
minimum = Convert.nonemptyFold Nonempty.minimum

{-| The least input with respect to the given comparison function -}
minimumBy :: (a -> a -> Ordering) -> Monad m => EffectfulFold m a (Maybe a)
minimumBy cmp = Convert.nonemptyFold (Nonempty.minimumBy cmp)

{-| Start with 'mempty', append each input on the right with ('<>') -}
monoid :: Monoid a => Monad m => EffectfulFold m a a
monoid = Convert.fold Pure.monoid

{-| 'True' if the input contains no inputs -}
null :: Monad m => EffectfulFold m a Bool
null = Convert.shortcutFold Shortcut.null

{-| The number of inputs -}
length :: Monad m => EffectfulFold m a Natural
length = Convert.fold Pure.length

{-| 'True' if all inputs are 'True' -}
and :: Monad m => EffectfulFold m Bool Bool
and = Convert.shortcutFold Shortcut.and

{-| 'True' if any input is 'True' -}
or :: Monad m => EffectfulFold m Bool Bool
or = Convert.shortcutFold Shortcut.or

{-| 'True' if all inputs satisfy the predicate -}
all :: Monad m => (a -> Bool) -> EffectfulFold m a Bool
all predicate = Convert.shortcutFold (Shortcut.all predicate)

{-| 'True' if any input satisfies the predicate -}
any :: Monad m => (a -> Bool) -> EffectfulFold m a Bool
any predicate = Convert.shortcutFold (Shortcut.any predicate)

{-| Adds the inputs -}
sum :: Num a => Monad m => EffectfulFold m a a
sum = Convert.fold Pure.sum

{-| Multiplies the inputs -}
product :: Num a => Monad m => EffectfulFold m a a
product = Convert.fold Pure.product

{-| Numerically stable arithmetic mean of the inputs -}
mean :: Fractional a => Monad m => EffectfulFold m a a
mean = Convert.fold Pure.mean

{-| Numerically stable (population) variance over the inputs -}
variance :: Fractional a => Monad m => EffectfulFold m a a
variance = Convert.fold Pure.variance

{-| Numerically stable (population) standard deviation over the inputs -}
standardDeviation :: Floating a => Monad m => EffectfulFold m a a
standardDeviation = Convert.fold Pure.standardDeviation

{-| 'True' if any input is equal to the given value -}
element :: Eq a => Monad m => a -> EffectfulFold m a Bool
element a = Convert.shortcutFold (Shortcut.element a)

{-| 'False' if any input is equal to the given value -}
notElement :: Eq a => Monad m => a -> EffectfulFold m a Bool
notElement a = Convert.shortcutFold (Shortcut.notElement a)

{-| The first input that satisfies the predicate, if any -}
find :: Monad m => (a -> Bool) -> EffectfulFold m a (Maybe a)
find ok = Convert.shortcutFold (Shortcut.find ok)

{-| The /n/th input, where n=0 is the first input, if the index is in bounds -}
index :: Monad m => Natural -> EffectfulFold m a (Maybe a)
index i = Convert.shortcutFold (Shortcut.index i)

{-| The index of the first input that matches the given value, if any -}
elementIndex :: Eq a => Monad m => a -> EffectfulFold m a (Maybe Natural)
elementIndex a = Convert.shortcutFold (Shortcut.elementIndex a)

{-| The index of the first input that satisfies the predicate, if any -}
findIndex :: Monad m => (a -> Bool) -> EffectfulFold m a (Maybe Natural)
findIndex ok = Convert.shortcutFold (Shortcut.findIndex ok)

{-| The @b@ from the first tuple where @a@ equals the given value, if any -}
lookup :: Eq a => Monad m => a -> EffectfulFold m (a, b) (Maybe b)
lookup a = Convert.shortcutFold (Shortcut.lookup a)

{-| All the inputs -}
list :: Monad m => EffectfulFold m a [a]
list = Convert.fold Pure.list

{-| All the inputs in reverse order -}
reverseList :: Monad m => EffectfulFold m a [a]
reverseList = Convert.fold Pure.reverseList
