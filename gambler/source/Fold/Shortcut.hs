module Fold.Shortcut
  (
    {- * Type -} ShortcutFold (..),

    {- * Run -} run,

    {- * Examples -}
    {- ** Length -} null, length,
    {- ** Boolean -} and, or, all, any,
    {- ** Numeric -} sum, product, mean, variance, standardDeviation,
    {- ** Search -} element, notElement, find, lookup,
    {- ** Index -} index, findIndex, elementIndex,
    {- ** List -} list, reverseList,
    {- ** General -} magma, semigroup, monoid,
    {- ** Endpoints -} first, last,
    {- ** Extrema -} maximum, minimum, maximumBy, minimumBy,

    {- * Conversion -} fold, effectfulFold, nonemptyFold, shortcutNonemptyFold,

    {- * Utilities -} demotivate,
  )
  where

import Fold.Shortcut.Conversion
import Fold.Shortcut.Examples
import Fold.Shortcut.Run
import Fold.Shortcut.Type
import Fold.Shortcut.Utilities
