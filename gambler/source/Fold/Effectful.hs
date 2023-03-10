module Fold.Effectful
  (
    {- * Type -} EffectfulFold (..),

    {- * Run -} run,

    {- * Examples -}
    {- ** General -} effect, effectMonoid, magma, semigroup, monoid,
    {- ** Endpoints -} first, last,
    {- ** Extrema -} maximum, minimum, maximumBy, minimumBy,
    {- ** Length -} null, length,
    {- ** Boolean -} and, or, all, any,
    {- ** Numeric -} sum, product, mean, variance, standardDeviation,
    {- ** Search -} element, notElement, find, lookup,
    {- ** Index -} index, findIndex, elementIndex,
    {- ** List -} list, reverseList,

    {- * Conversion -} fold, nonemptyFold, shortcutFold, shortcutNonemptyFold,

    {- * Utilities -} hoist, duplicate, premap, prefilter, drop,
  )
  where

import Fold.Effectful.Conversion
import Fold.Effectful.Examples
import Fold.Effectful.Run
import Fold.Effectful.Type
import Fold.Effectful.Utilities
