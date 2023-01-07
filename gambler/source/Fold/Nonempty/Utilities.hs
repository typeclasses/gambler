module Fold.Nonempty.Utilities where

import Fold.Nonempty.Type

import Control.Applicative (Applicative, liftA2)
import Data.Functor (fmap)
import Fold.Pure.Type (Fold (Fold))

import qualified Fold.Pure.Type as Pure

{-| Allows to continue feeding a fold even after passing it to a function
that closes it -}
duplicate :: NonemptyFold a b -> NonemptyFold a (Fold a b)
duplicate NonemptyFold{ initial, step, extract } =
    NonemptyFold{ initial, step, extract = \x -> Fold
        { Pure.initial = x, Pure.step, Pure.extract } }

{-| @(premap f folder)@ returns a new fold where @f@ is applied at each step -}
premap :: (a -> b) -> NonemptyFold b r -> NonemptyFold a r
premap f NonemptyFold{ initial, step, extract } =
    NonemptyFold{ initial = \a -> initial (f a),
        step = \x a -> step x (f a), extract }

{-| Nest a fold in an applicative -}
nest :: Applicative f => NonemptyFold a b -> NonemptyFold (f a) (f b)
nest NonemptyFold{ initial, step, extract } = NonemptyFold
    { initial = fmap initial, step = liftA2 step, extract = fmap extract }
