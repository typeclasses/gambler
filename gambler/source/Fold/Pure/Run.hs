module Fold.Pure.Run where

import Fold.Pure.Type

import Data.Foldable (Foldable)
import Data.Traversable (Traversable, mapAccumL)
import Prelude (($!))

import qualified Data.Foldable as F

{-| Fold a listlike container to a single summary result

@
run 'Fold.Pure.Examples.monoid' ["a", "b", "c"] = "abc"
@ -}
run :: Foldable f => Fold a b -> f a -> b
run Fold{ initial, step, extract } as = F.foldr cons extract as initial
  where
    cons a k x = k $! step x a

{-| Rather than only obtain a single final result, scanning gives a running
total that shows the intermediate result at each step along the way

@
scan 'Fold.Pure.Examples.monoid' ["a", "b", "c"] = ["","a","ab","abc"]
@ -}
scan :: Foldable f => Fold a b -> f a -> [b]
scan Fold{ initial, step, extract } as = F.foldr cons nil as initial
  where
    nil x = extract x : []
    cons a k x = extract x : (k $! step x a)

{-| Scan where the last input is excluded

@
prescan 'Fold.Pure.Examples.monoid' ["a", "b", "c"] = ["","a","ab"]
@ -}
prescan :: Traversable t => Fold a b -> t a -> t b
prescan Fold{ initial, step, extract } as = bs
  where
    step' x a = (x', b)
      where
        x' = step x a
        b  = extract x
    (_, bs) = mapAccumL step' initial as

{-| Scan where the first input is excluded

@
postscan 'Fold.Pure.Examples.monoid' ["a", "b", "c"] = ["a","ab","abc"]
@ -}
postscan :: Traversable t => Fold a b -> t a -> t b
postscan Fold{ initial, step, extract } as = bs
  where
    step' x a = (x', b)
      where
        x' = step x a
        b  = extract x'
    (_, bs) = mapAccumL step' initial as
