{-# LANGUAGE StrictData #-}

-- | Strict data types for use as internal
-- accumulators to achieve constant space usage.
module Strict
  (
    {- * Maybe -} Maybe (..), lazy, strict,
    {- * Either -} Either (..), hush,
    {- * Tuples -} Tuple2 (..), Tuple3 (..),
  )
  where

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
