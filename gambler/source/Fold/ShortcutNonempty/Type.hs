module Fold.ShortcutNonempty.Type
  (
    ShortcutNonemptyFold (..),
    Will (..), Vitality (..), Vitality',
  )
  where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Strict (Will (..), Vitality (..), Vitality')
import Data.Void (absurd)

import qualified Strict

{-| Processes at least one input of type @a@, has the ability to halt
    midway through the stream, and results in a value of type @b@ -}
data ShortcutNonemptyFold a b = forall x y. ShortcutNonemptyFold
    { initial :: a -> Vitality x y
    , step :: y -> a -> Vitality x y
    , extract :: Vitality x y -> b
    }

instance Functor (ShortcutNonemptyFold a) where
    fmap f ShortcutNonemptyFold{ step, initial, extract } =
        ShortcutNonemptyFold{ initial, step, extract = \x -> f (extract x) }

instance Applicative (ShortcutNonemptyFold a) where
    pure b = ShortcutNonemptyFold
        { initial = \_ -> Dead ()
        , step = absurd
        , extract = \e -> case e of { Dead () -> b }
        }

    (<*>)
        ShortcutNonemptyFold{ initial = initialL, step = stepL, extract = extractL }
        ShortcutNonemptyFold{ initial = initialR, step = stepR, extract = extractR } =
          ShortcutNonemptyFold
            { initial = \a -> Strict.vitality2 (initialL a) (initialR a)
            , step = \(Strict.Tuple2 xL xR) a -> Strict.vitality2
                (Strict.unlessDead (\x -> stepL x a) xL)
                (Strict.unlessDead (\x -> stepR x a) xR)
            , extract = \e -> case e of { Dead x -> ex x; Alive _ x -> ex x }
            }
          where
            ex (Strict.Tuple2 xL xR) = (extractL xL) (extractR xR)

instance Semigroup b => Semigroup (ShortcutNonemptyFold a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (ShortcutNonemptyFold a b) where
    mempty = pure mempty
