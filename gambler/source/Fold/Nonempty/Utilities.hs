module Fold.Nonempty.Utilities where

import Fold.Nonempty.Type

import Control.Applicative (Applicative, liftA2)
import Data.Functor (fmap)
import Fold.Pure.Type (Fold (Fold))

import qualified Fold.Pure.Type as Pure
import qualified Fold.Pure.Utilities as Pure
import qualified Fold.Pure.Run as Pure.Run
import qualified Fold.Nonempty.Conversion as Nonempty

{-| Allows to continue feeding a fold even after passing it to a function
    that closes it -}
duplicate :: NonemptyFold a b -> NonemptyFold a (Fold a b)
duplicate NonemptyFold{ initial, step, extract } =
    NonemptyFold{ initial, step, extract = \x -> Fold
        { Pure.initial = x, Pure.step, Pure.extract } }

{-| Applies a function to each input before processing -}
premap :: (a -> b) -> NonemptyFold b r -> NonemptyFold a r
premap f NonemptyFold{ initial, step, extract } =
    NonemptyFold{ initial = \a -> initial (f a),
        step = \x a -> step x (f a), extract }

{-| Nest a fold in an applicative -}
nest :: Applicative f => NonemptyFold a b -> NonemptyFold (f a) (f b)
nest NonemptyFold{ initial, step, extract } = NonemptyFold
    { initial = fmap initial, step = liftA2 step, extract = fmap extract }

{-| Convert a nonempty fold for a single item (@x@) into a
    nonempty fold for nonempty lists of items (@xs@) -}
repeatedly :: forall x xs result.
    (forall b. NonemptyFold x b -> xs -> b)
        -- ^ A witness to the fact that @xs@ is a nonempty list of @x@
    -> NonemptyFold x result
    -> NonemptyFold xs result
repeatedly runXs foldX =
  NonemptyFold
    { initial = runXs (duplicate foldX)
    , step = \f -> runXs (Nonempty.fold (Pure.duplicate f))
    , extract = \f -> Pure.Run.run f []
    }
