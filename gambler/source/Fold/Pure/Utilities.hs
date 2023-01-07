module Fold.Pure.Utilities where

import Fold.Pure.Type

import Control.Applicative (Applicative, liftA2, pure)
import Data.Bool (Bool (False, True), (&&))
import Data.Functor (fmap)
import Numeric.Natural (Natural)
import Prelude ((-))

import qualified Strict

{-| Allows to continue feeding a fold even after passing it to a function
that closes it -}
duplicate :: Fold a b -> Fold a (Fold a b)
duplicate Fold{ initial, step, extract } =
    Fold{ initial, step, extract = \x -> Fold{ initial = x, step, extract } }

{-| Applies a function to each input before processing -}
premap :: (a -> b) -> Fold b r -> Fold a r
premap f Fold{ initial, step, extract } =
    Fold{ initial, step = \x a -> step x (f a), extract }

{-| Consider only inputs that match a predicate -}
prefilter :: (a -> Bool) -> Fold a r -> Fold a r
prefilter f Fold{ step, initial, extract } =
    Fold{ initial, step = \x a -> if f a then step x a else x, extract }

{-| Ignores inputs until they stop satisfying a predicate -}
predropWhile :: (a -> Bool) -> Fold a r -> Fold a r
predropWhile f Fold{ initial, step, extract } = Fold
    { initial = Strict.Tuple2 True initial
    , step = \(Strict.Tuple2 dropping x) a ->
          if dropping && f a
          then Strict.Tuple2 True x
          else Strict.Tuple2 False (step x a)
    , extract = \(Strict.Tuple2 _ state) -> extract state
    }

{-| Ignores the first /n/ inputs -}
drop :: Natural -> Fold a b -> Fold a b
drop n Fold{ initial, step, extract } = Fold
    { initial = (n, initial)
    , step = \(n', s) x -> case n' of
          0 -> (0, step s x)
          _ -> (n' - 1, s)
    , extract = \(_,  s) -> extract s
    }

{-| Nest a fold in an applicative -}
nest :: Applicative f => Fold a b -> Fold (f a) (f b)
nest Fold{ initial, step, extract } = Fold
    { initial = pure initial, step = liftA2 step, extract = fmap extract }
