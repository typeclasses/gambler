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
import Data.Bool (Bool)
import Data.Eq (Eq, (/=), (==))
import Data.Functor (($>), (<&>))
import Data.Maybe (Maybe (Just, Nothing))
import Fold.Shortcut.Conversion (fold)
import Fold.Shortcut.Utilities (demotivate)
import Numeric.Natural (Natural)
import Prelude ((-))
import Strict (isAlive, isDead)

import qualified Fold.Pure.Examples.Interesting as Fold

{-| 'True' if the input contains no inputs (tenacious) -}
null :: ShortcutFold a Bool
null = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \() _ -> Dead ()
  , extract = isAlive
  }

{-| 'True' if all inputs are 'True' (tenacious) -}
and :: ShortcutFold Bool Bool
and = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \_ a -> if a then Alive Tenacious () else Dead ()
  , extract = isAlive
  }

{-| 'True' if any input is 'True' (tenacious) -}
or :: ShortcutFold Bool Bool
or = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \() a -> if a then Dead () else Alive Tenacious ()
  , extract = isDead
  }

{-| 'True' if all inputs satisfy the predicate (tenacious) -}
all :: (a -> Bool) -> ShortcutFold a Bool
all predicate = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \() a -> if predicate a then Alive Tenacious () else Dead ()
  , extract = isAlive
  }

{-| 'True' if any input satisfies the predicate (tenacious) -}
any :: (a -> Bool) -> ShortcutFold a Bool
any predicate = ShortcutFold
  { initial = Alive Tenacious ()
  , step = \_ a -> if predicate a then Dead () else Alive Tenacious ()
  , extract = isDead
  }

{-| 'True' if any input is equal to the given value (tenacious) -}
element :: Eq a => a -> ShortcutFold a Bool
element a = any (a ==)

{-| 'False' if any input is equal to the given value (tenacious) -}
notElement :: Eq a => a -> ShortcutFold a Bool
notElement a = all (a /=)

{-| The first input that satisfies the predicate, if any (tenacious) -}
find :: (a -> Bool) -> ShortcutFold a (Maybe a)
find ok = ShortcutFold
    { initial = Alive Tenacious ()
    , step = \() a -> if ok a then Dead a else Alive Tenacious ()
    , extract = \v -> case v of { Dead x -> Just x; _ -> Nothing }
    }

{-| The /n/th input, where n=0 is the first input, if the index is in
    bounds (tenacious) -}
index :: Natural -> ShortcutFold a (Maybe a)
index i = ShortcutFold
    { initial = Alive Tenacious i
    , step = \i' a -> if i' == 0 then Dead a else Alive Tenacious (i' - 1)
    , extract = \v -> case v of { Dead x -> Just x; _ -> Nothing }
    }

{-| The index of the first input that matches the given value, if any
    (tenacious) -}
elementIndex :: Eq a => a -> ShortcutFold a (Maybe Natural)
elementIndex a = findIndex (a ==)

{-| The index of the first input that satisfies the predicate, if any
    (tenacious) -}
findIndex :: (a -> Bool) -> ShortcutFold a (Maybe Natural)
findIndex ok = demotivate
  (
    liftA2 (,) (fold Fold.length) (find ok)
    <&> \(n, found) -> found $> (n - 1)
  )

{-| The @b@ from the first tuple where @a@ equals the given value,
    if any (tenacious) -}
lookup :: Eq a => a -> ShortcutFold (a, b) (Maybe b)
lookup a0 = ShortcutFold
    { initial = Alive Tenacious ()
    , step = \() (a, b) -> if a == a0 then Dead b else Alive Tenacious ()
    , extract = \v -> case v of { Dead x -> Just x; _ -> Nothing }
    }
