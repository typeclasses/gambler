-- | Folds of other types trivially lifted into 'NonemptyFold'
module Fold.Nonempty.Examples.Boring
  (
    {- * Endpoints -} first,
    {- * Monoid -} monoid,
    {- * Length -} length,
    {- * Boolean -} and, or, all, any,
    {- * Numeric -} mean, variance, standardDeviation,
    {- * Search -} element, notElement, find, lookup,
    {- * Index -} index, findIndex, elementIndex,
    {- * List -} list, reverseList,
  )
  where

import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid)
import Fold.Nonempty.Type (NonemptyFold)
import Numeric.Natural (Natural)
import Prelude (Floating, Fractional)

import qualified Fold.Nonempty.Conversion as Convert
import qualified Fold.Pure.Examples.Interesting as Pure
import qualified Fold.Shortcut.Examples.Interesting as Shortcut
import qualified Fold.ShortcutNonempty.Examples as ShortcutNonempty

{-| The first input -}
first :: NonemptyFold a a
first = Convert.shortcutNonemptyFold ShortcutNonempty.first

{-| Start with 'mempty', append each input on the right with ('<>') -}
monoid :: Monoid a => NonemptyFold a a
monoid = Convert.fold Pure.monoid

{-| The number of inputs -}
length :: NonemptyFold a Natural
length = Convert.fold Pure.length

{-| 'True' if all inputs are 'True' -}
and :: NonemptyFold Bool Bool
and = Convert.shortcutFold Shortcut.and

{-| 'True' if any input is 'True' -}
or :: NonemptyFold Bool Bool
or = Convert.shortcutFold Shortcut.or

{-| 'True' if all inputs satisfy the predicate -}
all :: (a -> Bool) -> NonemptyFold a Bool
all predicate = Convert.shortcutFold (Shortcut.all predicate)

{-| 'True' if any input satisfies the predicate -}
any :: (a -> Bool) -> NonemptyFold a Bool
any predicate = Convert.shortcutFold (Shortcut.any predicate)

{-| Numerically stable arithmetic mean of the inputs -}
mean :: Fractional a => NonemptyFold a a
mean = Convert.fold Pure.mean

{-| Numerically stable (population) variance over the inputs -}
variance :: Fractional a => NonemptyFold a a
variance = Convert.fold Pure.variance

{-| Numerically stable (population) standard deviation over the inputs -}
standardDeviation :: Floating a => NonemptyFold a a
standardDeviation = Convert.fold Pure.standardDeviation

{-| 'True' if any input is equal to the given value -}
element :: Eq a => a -> NonemptyFold a Bool
element a = Convert.shortcutFold (Shortcut.element a)

{-| 'False' if any input is equal to the given value -}
notElement :: Eq a => a -> NonemptyFold a Bool
notElement a = Convert.shortcutFold (Shortcut.notElement a)

{-| The first input that satisfies the predicate, if any -}
find :: (a -> Bool) -> NonemptyFold a (Maybe a)
find ok = Convert.shortcutFold (Shortcut.find ok)

{-| The /n/th input, where n=0 is the first input, if the index is in bounds -}
index :: Natural -> NonemptyFold a (Maybe a)
index i = Convert.shortcutFold (Shortcut.index i)

{-| The index of the first input that matches the given value, if any -}
elementIndex :: Eq a => a -> NonemptyFold a (Maybe Natural)
elementIndex a = Convert.shortcutFold (Shortcut.elementIndex a)

{-| The index of the first input that satisfies the predicate, if any -}
findIndex :: (a -> Bool) -> NonemptyFold a (Maybe Natural)
findIndex ok = Convert.shortcutFold (Shortcut.findIndex ok)

{-| The @b@ from the first tuple where @a@ equals the given value, if any -}
lookup :: Eq a => a -> NonemptyFold (a, b) (Maybe b)
lookup a = Convert.shortcutFold (Shortcut.lookup a)

{-| All the inputs -}
list :: NonemptyFold a [a]
list = Convert.fold Pure.list

{-| All the inputs in reverse order -}
reverseList :: NonemptyFold a [a]
reverseList = Convert.fold Pure.reverseList
