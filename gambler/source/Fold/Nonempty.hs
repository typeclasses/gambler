module Fold.Nonempty
  (
    {- * Type -} NonemptyFold (..),

    {- * Run -} run,

    {- * Examples -}
    {- ** General -} magma, semigroup,
    {- ** Endpoints -} first, last,
    {- ** Extrema -} maximum, minimum, maximumBy, minimumBy,
    {- ** Pure -}
    {- *** Monoid -} monoid,
    {- *** Length -} null, length,
    {- *** Boolean -} and, or, all, any,
    {- *** Numeric -} sum, product, mean, variance, standardDeviation,
    {- *** Search -} element, notElement, find, lookup,
    {- *** Index -} index, findIndex, elementIndex,
    {- *** List -} list, reverseList,

    {- * Conversion -} fold, effectfulFold, shortcutFold, shortcutNonemptyFold,

    {- * Utilities -} duplicate, premap, nest,
  )
  where

import Fold.Nonempty.Conversion
import Fold.Nonempty.Examples
import Fold.Nonempty.Pure hiding (list, reverseList)
import Fold.Nonempty.Run
import Fold.Nonempty.Type
import Fold.Nonempty.Utilities
