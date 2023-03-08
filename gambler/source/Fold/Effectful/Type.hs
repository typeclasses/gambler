module Fold.Effectful.Type where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Control.Monad (Monad)
import Data.Functor (Functor, fmap, (<$>))
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Prelude (($!))

import qualified Strict

{-| Processes inputs of type @a@ and results in an effectful value of type @m b@ -}
data EffectfulFold m a b = forall x. EffectfulFold
    { initial :: m x
    , step :: x -> a -> m x
    , extract :: x -> m b
    }

instance Functor m => Functor (EffectfulFold m a) where
    fmap f EffectfulFold{ initial, step, extract } = EffectfulFold
        { initial
        , step
        , extract = \x -> fmap f $! extract x
        }

instance Applicative m => Applicative (EffectfulFold m a) where
    pure b = EffectfulFold{ initial = pure (), step = \() _ -> pure (), extract = \() -> pure b }

    (<*>)
        EffectfulFold{ initial = initialL, step = stepL, extract = extractL }
        EffectfulFold{ initial = initialR, step = stepR, extract = extractR } =
          EffectfulFold
            { initial = Strict.Tuple2 <$> initialL <*> initialR
            , step = \(Strict.Tuple2 xL xR) a -> Strict.Tuple2 <$> stepL xL a <*> stepR xR a
            , extract = \(Strict.Tuple2 xL xR) -> extractL xL <*> extractR xR
            }

instance (Semigroup b, Monad m) => Semigroup (EffectfulFold m a b) where
    (<>) = liftA2 (<>)

instance (Monoid b, Monad m) => Monoid (EffectfulFold m a b) where
    mempty = pure mempty
