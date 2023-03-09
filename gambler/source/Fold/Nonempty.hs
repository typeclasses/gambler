module Fold.Nonempty
  (
    {- * Type -} NonemptyFold (..),

    {- * Run -} run,

    {- * Examples -}
    {- ** General -} magma, semigroup, monoid,
    {- ** Endpoints -} first, last,
    {- ** Extrema -} maximum, minimum, maximumBy, minimumBy,
    {- ** Length -} length,
    {- ** Boolean -} and, or, all, any,
    {- ** Numeric -} sum, product, mean, variance, standardDeviation,
    {- ** Search -} element, notElement, find, lookup,
    {- ** Index -} index, findIndex, elementIndex,
    {- ** List -} list, reverseList,

    {- * Conversion -} fold, effectfulFold, shortcutFold, shortcutNonemptyFold,

    {- * Utilities -} duplicate, repeatedly, premap, nest,
  )
  where

import Fold.Nonempty.Conversion
import Fold.Nonempty.Examples
import Fold.Nonempty.Run
import Fold.Nonempty.Type
import Fold.Nonempty.Utilities
