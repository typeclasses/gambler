-- | Some interesting examples of effectful folds
module Fold.Effectful.Examples.Interesting where

import Fold.Effectful.Type

import Control.Monad (Monad)
import Data.Functor (void)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup ((<>))
import Prelude (($!))

import qualified Control.Applicative as Applicative

{-| Performs an action for each input, discarding the result -}
effect :: Monad m => (a -> m b) -> EffectfulFold m a ()
effect f = effectMonoid (\a -> void (f a))

{-| Performs an action for each input, monoidally combining the results
    from all the actions -}
effectMonoid ::  (Monoid w, Monad m) => (a -> m w) -> EffectfulFold m a w
effectMonoid act = EffectfulFold
    { initial = Applicative.pure mempty
    , step = \m a -> do{ m' <- act a; Applicative.pure $! (<>) m m' }
    , extract = Applicative.pure
    }
