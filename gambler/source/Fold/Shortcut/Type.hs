module Fold.Shortcut.Type
  (
    ShortcutFold (..),
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

{- | Processes inputs of type @a@, has the ability to halt midway
     through the stream, and results in a value of type @b@ -}
data ShortcutFold a b = forall x y. ShortcutFold
    { initial :: Vitality x y
    , step :: y -> a -> Vitality x y
    , extractDead :: x -> b
    , extractLive :: y -> b
    }

instance Functor (ShortcutFold a) where
    fmap f ShortcutFold{ step, initial, extractDead, extractLive } =
        ShortcutFold
          { initial
          , step
          , extractDead = \x -> f (extractDead x)
          , extractLive = \x -> f (extractLive x)
          }

instance Applicative (ShortcutFold a) where
    pure b = ShortcutFold
        { initial = Dead ()
        , step = absurd
        , extractDead = \() -> b
        , extractLive = absurd
        }

    (<*>)
        ShortcutFold{ initial = initialL, step = stepL, extractDead = extractDeadL, extractLive = extractLiveL }
        ShortcutFold{ initial = initialR, step = stepR, extractDead = extractDeadR, extractLive = extractLiveR } =
          ShortcutFold
            { initial = Strict.vitality2 initialL initialR
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

instance Semigroup b => Semigroup (ShortcutFold a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (ShortcutFold a b) where
    mempty = pure mempty
