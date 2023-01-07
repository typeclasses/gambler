-- | Folds from "Fold.Pure.Examples" trivially lifted into 'NonemptyFold'
module Fold.Nonempty.Pure
  (
    {- * Monoid -} monoid,
    {- * Length -} null, length,
    {- * Boolean -} and, or, all, any,
    {- * Numeric -} sum, product, mean, variance, standardDeviation,
    {- * Search -} element, notElement, find, lookup,
    {- * Index -} index, findIndex, elementIndex,
    {- * List -} list, reverseList,
  )
  where

import qualified Fold.Pure.Examples as Pure

import Data.Bool (Bool)
import Data.Eq (Eq)
import Data.Maybe (Maybe)
import Data.Monoid (Monoid)
import Fold.Nonempty.Conversion (fold)
import Fold.Nonempty.Type (NonemptyFold)
import Numeric.Natural (Natural)
import Prelude (Floating, Fractional, Num)

{-| Start with 'mempty', append each input on the right with ('<>') -}
monoid :: Monoid a => NonemptyFold a a
monoid = fold Pure.monoid

{-| 'True' if the input contains no inputs -}
null :: NonemptyFold a Bool
null = fold Pure.null

{-| The number of inputs -}
length :: NonemptyFold a Natural
length = fold Pure.length

{-| 'True' if all inputs are 'True' -}
and :: NonemptyFold Bool Bool
and = fold Pure.and

{-| 'True' if any input is 'True' -}
or :: NonemptyFold Bool Bool
or = fold Pure.or

{-| 'True' if all inputs satisfy the predicate -}
all :: (a -> Bool) -> NonemptyFold a Bool
all predicate = fold (Pure.all predicate)

{-| 'True' if any input satisfies the predicate -}
any :: (a -> Bool) -> NonemptyFold a Bool
any predicate = fold (Pure.any predicate)

{-| Adds the inputs -}
sum :: Num a => NonemptyFold a a
sum = fold Pure.sum

{-| Multiplies the inputs -}
product :: Num a => NonemptyFold a a
product = fold Pure.product

{-| Numerically stable arithmetic mean of the inputs -}
mean :: Fractional a => NonemptyFold a a
mean = fold Pure.mean

{-| Numerically stable (population) variance over the inputs -}
variance :: Fractional a => NonemptyFold a a
variance = fold Pure.variance

{-| Numerically stable (population) standard deviation over the inputs -}
standardDeviation :: Floating a => NonemptyFold a a
standardDeviation = fold Pure.standardDeviation

{-| 'True' if any input is equal to the given value -}
element :: Eq a => a -> NonemptyFold a Bool
element a = fold (Pure.element a)

{-| 'False' if any input is equal to the given value -}
notElement :: Eq a => a -> NonemptyFold a Bool
notElement a = fold (Pure.notElement a)

{-| The first input that satisfies the predicate, if any -}
find :: (a -> Bool) -> NonemptyFold a (Maybe a)
find ok = fold (Pure.find ok)

{-| The /n/th input, where n=0 is the first input, if the index is in bounds -}
index :: Natural -> NonemptyFold a (Maybe a)
index i = fold (Pure.index i)

{-| The index of the first input that matches the given value, if any -}
elementIndex :: Eq a => a -> NonemptyFold a (Maybe Natural)
elementIndex a = fold (Pure.elementIndex a)

{-| The index of the first input that satisfies the predicate, if any -}
findIndex :: (a -> Bool) -> NonemptyFold a (Maybe Natural)
findIndex ok = fold (Pure.findIndex ok)

{-| The @b@ from the first tuple where @a@ equals the given value, if any -}
lookup :: Eq a => a -> NonemptyFold (a, b) (Maybe b)
lookup a = fold (Pure.lookup a)

{-| All the inputs -}
list :: NonemptyFold a [a]
list = fold Pure.list

{-| All the inputs in reverse order -}
reverseList :: NonemptyFold a [a]
reverseList = fold Pure.reverseList
