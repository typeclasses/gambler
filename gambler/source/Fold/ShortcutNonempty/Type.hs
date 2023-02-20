module Fold.ShortcutNonempty.Type
  (
    ShortcutNonemptyFold (..),
    Will (..), Vitality (..),
  )
  where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Strict (Will (..), Vitality (..))
import Data.Void (absurd)

import qualified Strict

{- | Processes at least one input of type @a@, has the ability to halt
     midway through the stream, and results in a value of type @b@ -}
data ShortcutNonemptyFold a b = forall x y. ShortcutNonemptyFold
    { initial :: a -> Vitality x y
    , step :: y -> a -> Vitality x y
    , extractDead :: x -> b
    , extractLive :: y -> b
    }

instance Functor (ShortcutNonemptyFold a) where
    fmap f ShortcutNonemptyFold{ step, initial, extractDead, extractLive } =
        ShortcutNonemptyFold
          { initial
          , step
          , extractDead = \x -> f (extractDead x)
          , extractLive = \x -> f (extractLive x)
          }

instance Applicative (ShortcutNonemptyFold a) where
    pure b = ShortcutNonemptyFold
        { initial = \_ -> Dead ()
        , step = absurd
        , extractDead = \() -> b
        , extractLive = absurd
        }

    (<*>)
        ShortcutNonemptyFold{ initial = initialL, step = stepL, extractDead = extractDeadL, extractLive = extractLiveL }
        ShortcutNonemptyFold{ initial = initialR, step = stepR, extractDead = extractDeadR, extractLive = extractLiveR } =
          ShortcutNonemptyFold
            { initial = \a -> Strict.vitality2 (initialL a) (initialR a)
            , step = \(Strict.Tuple2 xL xR) a -> Strict.vitality2
                (Strict.unlessDead (\x -> stepL x a) xL)
                (Strict.unlessDead (\x -> stepR x a) xR)
            , extractDead = extract
            , extractLive = extract
            }
          where
            extract(Strict.Tuple2 xL xR) = f x
              where
                f = case xL of { Dead a -> extractDeadL a; Alive _ b -> extractLiveL b }
                x = case xR of { Dead a -> extractDeadR a; Alive _ b -> extractLiveR b }

instance Semigroup b => Semigroup (ShortcutNonemptyFold a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (ShortcutNonemptyFold a b) where
    mempty = pure mempty
