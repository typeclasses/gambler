module Fold.Pure.Examples
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

import Fold.Pure.Type

import Data.Bool (Bool (False, True), (&&), (||))
import Data.Eq (Eq, (/=), (==))
import Data.Function (id, ($), (.))
import Data.Functor ((<$>))
import Data.Maybe (Maybe)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)
import Prelude (Floating, Fractional, Num, sqrt, (*), (+), (-), (/))

import qualified Strict

{-| Start with 'mempty', append each input on the right with ('<>') -}
monoid :: Monoid a => Fold a a
monoid = Fold{ initial = mempty, step = (<>), extract = id }

{-| 'True' if the input contains no inputs -}
null :: Fold a Bool
null = Fold{ initial = True, step = \_ _ -> False, extract = id }

{-| The number of inputs -}
length :: Fold a Natural
length = Fold{ initial = 0, step = \n _ -> n + 1, extract = id }

{-| 'True' if all inputs are 'True' -}
and :: Fold Bool Bool
and = Fold{ initial = True, step = (&&), extract = id }

{-| 'True' if any input is 'True' -}
or :: Fold Bool Bool
or = Fold{ initial = False, step = (||), extract = id }

{-| 'True' if all inputs satisfy the predicate -}
all :: (a -> Bool) -> Fold a Bool
all predicate =
    Fold{ initial = True, step = \x a -> x && predicate a, extract = id }

{-| 'True' if any input satisfies the predicate -}
any :: (a -> Bool) -> Fold a Bool
any predicate =
    Fold{ initial = False, step = \x a -> x || predicate a, extract = id }

{-| Adds the inputs -}
sum :: Num a => Fold a a
sum = Fold{ initial = 0, step = (+), extract = id }

{-| Multiplies the inputs -}
product :: Num a => Fold a a
product = Fold{ initial = 1, step = (*), extract = id }

{-| Numerically stable arithmetic mean of the inputs -}
mean :: Fractional a => Fold a a
mean = Fold
    { initial = Strict.Tuple2 0 0
    , step = \(Strict.Tuple2 x n) y ->
        let n' = n + 1 in
        Strict.Tuple2 (x + (y - x) / n') n'
    , extract = \(Strict.Tuple2 x _) -> x
    }

{-| Numerically stable (population) variance over the inputs -}
variance :: Fractional a => Fold a a
variance = Fold
    { initial = Strict.Tuple3 0 0 0
    , step = \(Strict.Tuple3 n mean_ m2) x ->
        let
          n'     = n + 1
          mean'  = (n * mean_ + x) / (n + 1)
          delta  = x - mean_
          m2'    = m2 + delta * delta * n / (n + 1)
        in
          Strict.Tuple3 n' mean' m2'
    , extract = \(Strict.Tuple3 n _ m2) -> m2 / n
    }

{-| Numerically stable (population) standard deviation over the inputs -}
standardDeviation :: Floating a => Fold a a
standardDeviation = sqrt <$> variance

{-| 'True' if any input is equal to the given value -}
element :: Eq a => a -> Fold a Bool
element a = any (a ==)

{-| 'False' if any input is equal to the given value -}
notElement :: Eq a => a -> Fold a Bool
notElement a = all (a /=)

{-| The first input that satisfies the predicate, if any -}
find :: (a -> Bool) -> Fold a (Maybe a)
find ok = Fold
    { initial = Strict.Nothing
    , step = \x a -> case x of
        Strict.Nothing -> if ok a then Strict.Just a else Strict.Nothing
        _ -> x
    , extract = Strict.lazy
    }

{-| The /n/th input, where n=0 is the first input, if the index is in bounds -}
index :: Natural -> Fold a (Maybe a)
index i = Fold
    { initial = Strict.Left 0
    , step = \x a -> case x of
        Strict.Left j -> if i == j then Strict.Right a else Strict.Left (j + 1)
        _ -> x
    , extract = Strict.hush
    }

{-| The index of the first input that matches the given value, if any -}
elementIndex :: Eq a => a -> Fold a (Maybe Natural)
elementIndex a = findIndex (a ==)

{-| The index of the first input that satisfies the predicate, if any -}
findIndex :: (a -> Bool) -> Fold a (Maybe Natural)
findIndex ok = Fold
    { initial = Strict.Left 0
    , step = \x a -> case x of
        Strict.Left i -> if ok a then Strict.Right i else Strict.Left (i + 1)
        _ -> x
    , extract = Strict.hush
    }

{-| The @b@ from the first tuple where @a@ equals the given value, if any -}
lookup :: Eq a => a -> Fold (a, b) (Maybe b)
lookup a0 = Fold
    { initial = Strict.Nothing
    , step = \x (a, b) -> case x of
        Strict.Nothing -> if a == a0 then Strict.Just b else Strict.Nothing
        _ -> x
    , extract = Strict.lazy
    }

{-| All the inputs -}
list :: Fold a [a]
list = Fold{ initial = id, step = \x a -> x . (a :), extract = ($ []) }

{-| All the inputs in reverse order -}
reverseList :: Fold a [a]
reverseList = Fold{ initial = [], step = \x a -> a : x, extract = id }
