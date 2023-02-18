{-# LANGUAGE StrictData #-}

-- | Strict data types for use as internal
-- accumulators to achieve constant space usage.
module Strict
  (
    {- * Maybe -} Maybe (..), lazy, strict,
    {- * Either -} Either (..), hush,
    {- * Tuples -} Tuple2 (..), Tuple3 (..),
    {- * Shortcut -} Shortcut (..), Vitality (..), unlessDead, shortcut2,
  )
  where

import Data.Functor (Functor (..))
import Control.Applicative (Applicative (pure, (<*>)))
import Data.Semigroup (Semigroup, (<>))
import Data.Monoid (Monoid, mempty)

import qualified Data.Maybe as Lazy

data Maybe a = Just a | Nothing

instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> x = x
    x <> Nothing = x
    Just x <> Just y = Just (x <> y)

instance Semigroup a => Monoid (Maybe a) where
    mempty = Nothing

lazy :: Maybe a -> Lazy.Maybe a
lazy Nothing = Lazy.Nothing
lazy (Just a) = Lazy.Just a

strict :: Lazy.Maybe a -> Maybe a
strict Lazy.Nothing = Nothing
strict (Lazy.Just a) = Just a

data Either a b = Left a | Right b

hush :: Either a b -> Lazy.Maybe b
hush (Left _) = Lazy.Nothing
hush (Right b) = Lazy.Just b

data Tuple2 a b = Tuple2 a b

data Tuple3 a b c = Tuple3 a b c

data Vitality = Dead | Ambivalent | Alive

instance Semigroup Vitality where
    Alive <> _ = Alive
    _ <> Alive = Alive
    Ambivalent <> _ = Ambivalent
    _ <> Ambivalent = Ambivalent
    Dead <> Dead = Dead

instance Monoid Vitality where
    mempty = Dead

data Shortcut a = Shortcut{ vitality :: Vitality, shortcut :: a }

instance Functor Shortcut where
    fmap f (Shortcut v x) = Shortcut v (f x)

instance Applicative Shortcut where
    pure = Shortcut mempty
    Shortcut v1 f <*> Shortcut v2 x = Shortcut (v1 <> v2) (f x)

unlessDead :: forall x. (x -> Shortcut x) -> Shortcut x -> Shortcut x
unlessDead f s = case vitality s of { Dead -> s; _ -> f (shortcut s) }

shortcut2 :: forall x y. Shortcut x -> Shortcut y -> Shortcut (Strict.Tuple2 (Shortcut x) (Shortcut y))
shortcut2 a b = Shortcut (Strict.vitality a <> Strict.vitality b) (Strict.Tuple2 a b)
