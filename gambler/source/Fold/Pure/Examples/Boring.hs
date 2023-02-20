-- | Folds of other types trivially lifted into 'Fold'
module Fold.Pure.Examples.Boring
  (
    {- * Length -} null,
    {- * General -} magma, semigroup,
    {- * Endpoints -} first, last,
    {- * Extrema -} maximum, minimum, maximumBy, minimumBy,
    {- * Boolean -} and, or, all, any,
    {- * Search -} element, notElement, find, lookup,
    {- * Index -} index, findIndex, elementIndex,
  )
  where

import Data.Maybe (Maybe)
import Data.Ord (Ord, Ordering)
import Data.Semigroup (Semigroup)
import Fold.Pure.Type (Fold)
import Data.Bool (Bool)
import Data.Eq (Eq)
import Numeric.Natural (Natural)

import qualified Fold.Pure.Conversion as Convert
import qualified Fold.Nonempty.Examples.Interesting as Nonempty
import qualified Fold.ShortcutNonempty.Examples.Interesting as ShortcutNonempty
import qualified Fold.Shortcut.Examples.Interesting as Shortcut

{-| 'True' if the input contains no inputs -}
null :: Fold a Bool
null = Convert.shortcutFold Shortcut.null

{-| Start with the first input, append each new input on the right
with the given function -}
magma :: (a -> a -> a) -> Fold a (Maybe a)
magma step = Convert.nonemptyFold (Nonempty.magma step)

{-| Append each new input on the right with ('<>') -}
semigroup :: Semigroup a => Fold a (Maybe a)
semigroup = Convert.nonemptyFold Nonempty.semigroup

{-| The first input -}
first :: Fold a (Maybe a)
first = Convert.shortcutNonemptyFold ShortcutNonempty.first

{-| The last input -}
last :: Fold a (Maybe a)
last = Convert.nonemptyFold Nonempty.last

{-| The greatest input -}
maximum :: Ord a => Fold a (Maybe a)
maximum = Convert.nonemptyFold Nonempty.maximum

{-| The greatest input with respect to the given comparison function -}
maximumBy :: (a -> a -> Ordering) -> Fold a (Maybe a)
maximumBy cmp = Convert.nonemptyFold (Nonempty.maximumBy cmp)

{-| The least input -}
minimum :: Ord a => Fold a (Maybe a)
minimum = Convert.nonemptyFold Nonempty.minimum

{-| The least input with respect to the given comparison function -}
minimumBy :: (a -> a -> Ordering) -> Fold a (Maybe a)
minimumBy cmp = Convert.nonemptyFold (Nonempty.minimumBy cmp)

{-| 'True' if all inputs are 'True' -}
and :: Fold Bool Bool
and = Convert.shortcutFold Shortcut.and

{-| 'True' if any input is 'True' -}
or :: Fold Bool Bool
or = Convert.shortcutFold Shortcut.or

{-| 'True' if all inputs satisfy the predicate -}
all :: (a -> Bool) -> Fold a Bool
all predicate = Convert.shortcutFold (Shortcut.all predicate)

{-| 'True' if any input satisfies the predicate -}
any :: (a -> Bool) -> Fold a Bool
any predicate = Convert.shortcutFold (Shortcut.any predicate)

{-| 'True' if any input is equal to the given value -}
element :: Eq a => a -> Fold a Bool
element a = Convert.shortcutFold (Shortcut.element a)

{-| 'False' if any input is equal to the given value -}
notElement :: Eq a => a -> Fold a Bool
notElement a = Convert.shortcutFold (Shortcut.notElement a)

{-| The first input that satisfies the predicate, if any -}
find :: (a -> Bool) -> Fold a (Maybe a)
find ok = Convert.shortcutFold (Shortcut.find ok)

{-| The /n/th input, where n=0 is the first input, if the index is in bounds -}
index :: Natural -> Fold a (Maybe a)
index i = Convert.shortcutFold (Shortcut.index i)

{-| The index of the first input that matches the given value, if any -}
elementIndex :: Eq a => a -> Fold a (Maybe Natural)
elementIndex a = Convert.shortcutFold (Shortcut.elementIndex a)

{-| The index of the first input that satisfies the predicate, if any -}
findIndex :: (a -> Bool) -> Fold a (Maybe Natural)
findIndex ok = Convert.shortcutFold (Shortcut.findIndex ok)

{-| The @b@ from the first tuple where @a@ equals the given value, if any -}
lookup :: Eq a => a -> Fold (a, b) (Maybe b)
lookup a0 = Convert.shortcutFold (Shortcut.lookup a0)
