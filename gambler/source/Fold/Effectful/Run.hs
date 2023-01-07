module Fold.Effectful.Run where

import Fold.Effectful.Type

import Control.Monad (Monad)
import Data.Foldable (Foldable)
import Prelude (($!))

import qualified Data.Foldable as F

{-| Fold an listlike container to an action that produces a single summary
result -}
run :: Foldable f => Monad m => EffectfulFold m a b -> f a -> m b
run EffectfulFold{ initial, step, extract } as0 = do
    x0 <- initial
    F.foldr (\a k x -> do{ x' <- step x a; k $! x' }) extract as0 $! x0
