module Fold.Pure
  (
    {- * Type -} Fold (..),

    {- * Run -} run, scan, prescan, postscan,

    {- * Examples -}
    {- ** Monoid -} monoid,
    {- ** Length -} null, length,
    {- ** Boolean -} and, or, all, any,
    {- ** Numeric -} sum, product, mean, variance, standardDeviation,
    {- ** Search -} element, notElement, find, lookup,
    {- ** Index -} index, findIndex, elementIndex,
    {- ** List -} list, reverseList,
    {- ** Nonempty -}
    {- *** General -} magma, semigroup,
    {- *** Endpoints -} first, last,
    {- *** Extrema -} maximum, minimum, maximumBy, minimumBy,

    {- * Conversion -} effectfulFold, nonemptyFold, shortcutFold, shortcutNonemptyFold,

    {- * Utilities -} duplicate, premap, prefilter, predropWhile, drop, nest,
  )
  where

import Fold.Pure.Conversion
import Fold.Pure.Examples
import Fold.Pure.Nonempty
import Fold.Pure.Run
import Fold.Pure.Type
import Fold.Pure.Utilities
