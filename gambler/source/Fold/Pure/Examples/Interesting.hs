module Fold.Pure.Examples.Interesting
  (
    {- * Monoid -} monoid,
    {- * Length -} length,
    {- * Numeric -} sum, product, mean, variance, standardDeviation,
    {- * List -} list, reverseList,
  )
  where

import Fold.Pure.Type

import Data.Function (id, ($), (.))
import Data.Functor ((<$>))
import Data.Monoid (Monoid, mempty)
import Data.Semigroup ((<>))
import Numeric.Natural (Natural)
import Prelude (Floating, Fractional, Num, sqrt, (*), (+), (-), (/))

import qualified Strict

{-| Start with 'mempty', append each input on the right with ('<>') -}
monoid :: Monoid a => Fold a a
monoid = Fold{ initial = mempty, step = (<>), extract = id }

{-| The number of inputs -}
length :: Fold a Natural
length = Fold{ initial = 0, step = \n _ -> n + 1, extract = id }

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

{-| All the inputs -}
list :: Fold a [a]
list = Fold{ initial = id, step = \x a -> x . (a :), extract = ($ []) }

{-| All the inputs in reverse order -}
reverseList :: Fold a [a]
reverseList = Fold{ initial = [], step = \x a -> a : x, extract = id }
