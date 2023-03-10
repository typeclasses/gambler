module Fold.Effectful.Examples
  (
    {- * General -} effect, effectMonoid, magma, semigroup, monoid,
    {- * Endpoints -} first, last,
    {- * Extrema -} maximum, minimum, maximumBy, minimumBy,
    {- * Length -} null, length,
    {- * Boolean -} and, or, all, any,
    {- * Numeric -} sum, product, mean, variance, standardDeviation,
    {- * Search -} element, notElement, find, lookup,
    {- * Index -} index, findIndex, elementIndex,
    {- * List -} list, reverseList,
  )
  where

import Fold.Effectful.Examples.Interesting
import Fold.Effectful.Examples.Boring
