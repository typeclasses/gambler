module Fold.Nonempty.Examples
  (
    {- * General -} magma, semigroup, monoid,
    {- * Endpoints -} first, last,
    {- * Extrema -} maximum, minimum, maximumBy, minimumBy,
    {- * Length -} length,
    {- * Boolean -} and, or, all, any,
    {- * Numeric -} sum, product, mean, variance, standardDeviation,
    {- * Search -} element, notElement, find, lookup,
    {- * Index -} index, findIndex, elementIndex,
    {- * List -} list, reverseList,
  )
  where

import Fold.Nonempty.Examples.Interesting
import Fold.Nonempty.Examples.Boring hiding (list, reverseList)
