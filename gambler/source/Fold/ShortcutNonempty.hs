module Fold.ShortcutNonempty
  (
    {- * Type -} ShortcutNonemptyFold (..), Will (..), Vitality (..), Vitality',

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

    {- * Conversion -} fold, effectfulFold, nonemptyFold, shortcutFold,

    {- * Utilities -} motivate, demotivate, duplicate, repeatedly, premap, withVitality,
  )
  where

import Fold.ShortcutNonempty.Conversion
import Fold.ShortcutNonempty.Examples
import Fold.ShortcutNonempty.Run
import Fold.ShortcutNonempty.Type
import Fold.ShortcutNonempty.Utilities
