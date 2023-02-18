module Fold.ShortcutNonempty.Type
  (
    ShortcutNonemptyFold (..),
    Shortcut (..), Vitality (..),
  )
  where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Strict (Shortcut (..), Vitality (..))
import Prelude (undefined)

import qualified Strict

{- | Processes at least one input of type @a@, has the ability to halt
     midway through the stream, and results in a value of type @b@ -}
data ShortcutNonemptyFold a b = forall x. ShortcutNonemptyFold
    { initial :: a -> Shortcut x
    , step :: x -> a -> Shortcut x
    , extract :: x -> b
    }

instance Functor (ShortcutNonemptyFold a) where
    fmap f ShortcutNonemptyFold{ step, initial, extract } =
        ShortcutNonemptyFold{ initial, step, extract = \x -> f (extract x) }

instance Applicative (ShortcutNonemptyFold a) where
    pure b = ShortcutNonemptyFold
        { initial = \_ -> Shortcut Dead ()
        , step = undefined
        , extract = \() -> b
        }

    (<*>)
        ShortcutNonemptyFold{ initial = initialL, step = stepL, extract = extractL }
        ShortcutNonemptyFold{ initial = initialR, step = stepR, extract = extractR } =
          ShortcutNonemptyFold
            { initial = \a -> Strict.shortcut2 (initialL a) (initialR a)
            , step = \(Strict.Tuple2 xL xR) a -> Strict.shortcut2
                (Strict.unlessDead (\x -> stepL x a) xL)
                (Strict.unlessDead (\x -> stepR x a) xR)
            , extract = \(Strict.Tuple2 xL xR) ->
                  extractL (shortcut xL) (extractR (shortcut xR))
            }

instance Semigroup b => Semigroup (ShortcutNonemptyFold a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (ShortcutNonemptyFold a b) where
    mempty = pure mempty
