module Fold.Nonempty.Type where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))

import qualified Strict

{- | Processes at least one input of type @a@ and results in a value of type @b@ -}
data NonemptyFold a b = forall x. NonemptyFold
    { initial :: a -> x
    , step :: x -> a -> x
    , extract :: x -> b
    }

instance Functor (NonemptyFold a) where
    fmap f NonemptyFold{ step, initial, extract } =
        NonemptyFold{ initial, step, extract = \x -> f (extract x) }

instance Applicative (NonemptyFold a) where
    pure b = NonemptyFold{ initial = \_ -> (), step = \() _ -> (), extract = \() -> b }

    (<*>)
        NonemptyFold{ initial = initialL, step = stepL, extract = extractL }
        NonemptyFold{ initial = initialR, step = stepR, extract = extractR } =
          NonemptyFold
            { initial = \a -> Strict.Tuple2 (initialL a) (initialR a)
            , step = \(Strict.Tuple2 xL xR) a -> Strict.Tuple2 (stepL xL a) (stepR xR a)
            , extract = \(Strict.Tuple2 xL xR) -> extractL xL (extractR xR)
            }

instance Semigroup b => Semigroup (NonemptyFold a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (NonemptyFold a b) where
    mempty = pure mempty
