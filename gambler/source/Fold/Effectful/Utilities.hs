module Fold.Effectful.Utilities where

import Fold.Effectful.Type

import Control.Applicative (Applicative, pure)
import Control.Monad (Monad, (>>=))
import Data.Bool (Bool)
import Data.Functor (fmap)
import Numeric.Natural (Natural)
import Prelude ((-))

{-| Shift an effectful fold from one monad to another with a morphism such as
    @lift@ or @liftIO@ -}
hoist :: (forall x . m x -> n x) -> EffectfulFold m a b -> EffectfulFold n a b
hoist f EffectfulFold{ initial, step, extract } = EffectfulFold
    { initial = f initial
    , step = \a b -> f (step a b)
    , extract = \x -> f (extract x)
    }

{-| Allows to continue feeding an effectful fold even after passing it to a
    function that closes it -}
duplicate :: Applicative m => EffectfulFold m a b -> EffectfulFold m a (EffectfulFold m a b)
duplicate EffectfulFold{ initial, step, extract } = EffectfulFold
    { initial
    , step
    , extract = \x -> pure EffectfulFold{ initial = pure x, step, extract }
    }

{-| Apply a function to each input -}
premap :: Monad m => (a -> m b) -> EffectfulFold m b r -> EffectfulFold m a r
premap f EffectfulFold{ initial, step, extract } =
    EffectfulFold{ initial, step = \x a -> f a >>= step x, extract }

{-| Consider only inputs that match an effectful predicate -}
prefilter :: (Monad m) => (a -> m Bool) -> EffectfulFold m a r -> EffectfulFold m a r
prefilter f EffectfulFold{ initial, step, extract } = EffectfulFold
    { initial
    , step = \x a -> do{ use <- f a; if use then step x a else pure x }
    , extract
    }

{-| Ignore the first /n/ inputs -}
drop :: Monad m => Natural -> EffectfulFold m a b -> EffectfulFold m a b
drop n EffectfulFold{ initial, step, extract } = EffectfulFold
    { initial = fmap (\s -> (n, s)) initial
    , step = \(n', s) x -> case n' of
          0 -> fmap (\s' -> (0, s')) (step s x)
          _ -> pure (n' - 1, s)
    , extract = \(_,  s) -> extract s
    }
