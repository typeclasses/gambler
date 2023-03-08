{-# LANGUAGE StrictData #-}

-- | Strict data types for use as internal
-- accumulators to achieve constant space usage.
module Strict
  (
    {- * Maybe -} Maybe (..), lazy, strict,
    {- * Either -} Either (..), hush,
    {- * Tuples -} Tuple2 (..), Tuple3 (..),
    {- * Shortcut -} Vitality (..), Vitality', Will (..),
        unlessDead, vitality2, willSave, isAlive, isDead,
  )
  where

import Data.Bool (Bool (..))
import Data.Functor (Functor (..))
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))

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

data Will = Ambivalent | Tenacious

instance Semigroup Will where
    Tenacious <> _ = Tenacious
    _ <> Tenacious = Tenacious
    _ <> _ = Ambivalent

instance Monoid Will where
    mempty = Ambivalent

data Vitality a b = Dead a | Alive Will b
    deriving Functor

unlessDead :: (b -> Vitality a b) -> Vitality a b -> Vitality a b
unlessDead f s = case s of { Alive _ x -> f x; _ -> s }

willSave :: Vitality a b -> Vitality (Either a b) b
willSave v = case v of
    Dead x -> Dead (Left x)
    Alive Ambivalent x -> Dead (Right x)
    Alive Tenacious x -> Alive Tenacious x

type Vitality' a = Vitality a a

vitality2 :: Vitality a1 b1 -> Vitality a2 b2
    -> Vitality' (Strict.Tuple2 (Vitality a1 b1) (Vitality a2 b2))
vitality2 a@(Alive v1 _) b@(Alive v2 _) = Alive (v1 <> v2) (Strict.Tuple2 a b)
vitality2 a@(Dead _)     b@(Alive v _)  = Alive v          (Strict.Tuple2 a b)
vitality2 a@(Alive v _)  b@(Dead _)     = Alive v          (Strict.Tuple2 a b)
vitality2 a@(Dead _)     b@(Dead _)     = Dead             (Strict.Tuple2 a b)

isAlive :: Vitality a b -> Bool
isAlive v = case v of { Alive _ _ -> True; Dead _ -> False }

isDead :: Vitality a b -> Bool
isDead v = case v of { Alive _ _ -> False; Dead _ -> True }
