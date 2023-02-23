module Fold.Nonempty.Examples.Interesting
  (
    {- * General -} magma, semigroup,
    {- * Endpoints -} last,
    {- * Extrema -} maximum, minimum, maximumBy, minimumBy,
    {- * Numeric -} sum, product,
    {- * List -} list, reverseList,
  )
  where

import Fold.Nonempty.Type

import Data.Function (id, const, flip, (.))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Ord (Ord, Ordering (GT), max, min)
import Data.Semigroup (Semigroup, (<>))
import Prelude (Num, (+), (*))

import qualified Strict

{-| Start with the first input, append each new input on the right
with the given function -}
magma :: (a -> a -> a) -> NonemptyFold a a
magma step = NonemptyFold{ initial = id, step, extract = id }

{-| Append each new input on the right with ('<>') -}
semigroup :: Semigroup a => NonemptyFold a a
semigroup = magma (<>)

{-| The last input -}
last :: NonemptyFold a a
last = magma (flip const)

{-| The greatest input -}
maximum :: Ord a => NonemptyFold a a
maximum = magma max

{-| The greatest input with respect to the given comparison function -}
maximumBy :: (a -> a -> Ordering) -> NonemptyFold a a
maximumBy cmp = magma (\x y -> case cmp x y of { GT -> x; _ -> y })

{-| The least input -}
minimum :: Ord a => NonemptyFold a a
minimum = magma min

{-| The least input with respect to the given comparison function -}
minimumBy :: (a -> a -> Ordering) -> NonemptyFold a a
minimumBy cmp = magma (\x y -> case cmp x y of { GT -> y; _ -> x })

{-| Adds the inputs -}
sum :: Num a => NonemptyFold a a
sum = magma (+)

{-| Multiplies the inputs -}
product :: Num a => NonemptyFold a a
product = magma (*)

{-| All the inputs -}
list :: NonemptyFold a (NonEmpty a)
list = NonemptyFold
    { initial = \a -> Strict.Tuple2 a id
    , step = \(Strict.Tuple2 a0 x) a -> Strict.Tuple2 a0 (x . (a :))
    , extract = \(Strict.Tuple2 a0 x) -> a0 :| (x [])
    }

{-| All the inputs in reverse order -}
reverseList :: NonemptyFold a (NonEmpty a)
reverseList = NonemptyFold
    { initial = (:| [])
    , step = \(b :| x) a -> a :| b : x
    , extract = id
    }
