module Fold.Pure.Type where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))

import qualified Strict

{- | Processes inputs of type @a@ and results in a value of type @b@ -}
data Fold a b = forall x. Fold
    { initial :: x
    , step :: x -> a -> x
    , extract :: x -> b
    }

instance Functor (Fold a) where
    fmap f Fold{ initial, step, extract } =
        Fold{ initial, step, extract = \x -> f (extract x) }

instance Applicative (Fold a) where
    pure b = Fold{ initial = (), step = \() _ -> (), extract = \() -> b }

    (<*>)
        Fold{ initial = initialL, step = stepL, extract = extractL }
        Fold{ initial = initialR, step = stepR, extract = extractR } =
          Fold
            { initial = Strict.Tuple2 initialL initialR
            , step = \(Strict.Tuple2 xL xR) a -> Strict.Tuple2 (stepL xL a) (stepR xR a)
            , extract = \(Strict.Tuple2 xL xR) -> extractL xL (extractR xR)
            }

instance Semigroup b => Semigroup (Fold a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (Fold a b) where
    mempty = pure mempty
