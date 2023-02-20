module Fold.Shortcut.Examples.Interesting
  (
    {- * Length -} null,
    {- * Boolean -} and, or, all, any,
    {- * Search -} element, notElement, find, lookup,
    {- * Index -} index, findIndex, elementIndex,
  )
  where

import Fold.Shortcut.Type

import Control.Applicative (liftA2)
import Data.Bool (Bool (False, True))
import Data.Eq (Eq, (/=), (==))
import Data.Functor (($>), (<&>))
import Data.Maybe (Maybe (Just, Nothing))
import Numeric.Natural (Natural)
import Prelude ((-))
import Fold.Shortcut.Conversion (fold)
import Fold.Shortcut.Utilities (demotivate)

import qualified Fold.Pure.Examples.Interesting as Fold

{-| 'True' if the input contains no inputs -}
null :: ShortcutFold a Bool
null = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \() _ -> Dead ()
  , extractLive = \() -> True
  , extractDead = \() -> False
  }

{-| 'True' if all inputs are 'True' -}
and :: ShortcutFold Bool Bool
and = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \_ a -> if a then Alive Tenacious () else Dead ()
  , extractLive = \() -> True
  , extractDead = \() -> False
  }

{-| 'True' if any input is 'True' -}
or :: ShortcutFold Bool Bool
or = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \() a -> if a then Dead () else Alive Tenacious ()
  , extractLive = \() -> False
  , extractDead = \() -> True
  }

{-| 'True' if all inputs satisfy the predicate -}
all :: (a -> Bool) -> ShortcutFold a Bool
all predicate = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \() a -> if predicate a then Alive Tenacious () else Dead ()
  , extractLive = \() -> True
  , extractDead = \() -> False
  }

{-| 'True' if any input satisfies the predicate -}
any :: (a -> Bool) -> ShortcutFold a Bool
any predicate = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \_ a -> if predicate a then Dead () else Alive Tenacious ()
  , extractLive = \() -> False
  , extractDead = \() -> True
  }

{-| 'True' if any input is equal to the given value -}
element :: Eq a => a -> ShortcutFold a Bool
element a = any (a ==)

{-| 'False' if any input is equal to the given value -}
notElement :: Eq a => a -> ShortcutFold a Bool
notElement a = all (a /=)

{-| The first input that satisfies the predicate, if any -}
find :: (a -> Bool) -> ShortcutFold a (Maybe a)
find ok = ShortcutFold
    { initial = Alive Tenacious ()
    , step = \() a -> if ok a then Dead a else Alive Tenacious ()
    , extractDead = Just
    , extractLive = \() -> Nothing
    }

{-| The /n/th input, where n=0 is the first input, if the index is in bounds -}
index :: Natural -> ShortcutFold a (Maybe a)
index i = ShortcutFold
    { initial = Alive Tenacious i
    , step = \i' a -> if i' == 0 then Dead a else Alive Tenacious (i' - 1)
    , extractDead = Just
    , extractLive = \_ -> Nothing
    }

{-| The index of the first input that matches the given value, if any -}
elementIndex :: Eq a => a -> ShortcutFold a (Maybe Natural)
elementIndex a = findIndex (a ==)

{-| The index of the first input that satisfies the predicate, if any -}
findIndex :: (a -> Bool) -> ShortcutFold a (Maybe Natural)
findIndex ok = demotivate
  (
    liftA2 (,) (fold Fold.length) (find ok)
    <&> \(n, found) -> found $> (n - 1)
  )

{-| The @b@ from the first tuple where @a@ equals the given value, if any -}
lookup :: Eq a => a -> ShortcutFold (a, b) (Maybe b)
lookup a0 = ShortcutFold
    { initial = Alive Tenacious ()
    , step = \() (a, b) -> if a == a0 then Dead b else Alive Tenacious ()
    , extractLive = \() -> Nothing
    , extractDead = Just
    }
