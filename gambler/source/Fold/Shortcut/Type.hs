module Fold.Shortcut.Type
  (
    ShortcutFold (..),
    Shortcut (..), Vitality (..),
  )
  where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Strict (Shortcut (..), Vitality (..))
import Prelude (undefined)

import qualified Strict

{- | Processes inputs of type @a@, has the ability to halt midway
     through the stream, and results in a value of type @b@ -}
data ShortcutFold a b = forall x. ShortcutFold
    { initial :: Shortcut x
    , step :: x -> a -> Shortcut x
    , extract :: x -> b
    }

instance Functor (ShortcutFold a) where
    fmap f ShortcutFold{ step, initial, extract } =
        ShortcutFold{ initial, step, extract = \x -> f (extract x) }

instance Applicative (ShortcutFold a) where
    pure b = ShortcutFold
        { initial = Shortcut Dead ()
        , step = undefined
        , extract = \() -> b
        }

    (<*>)
        ShortcutFold{ initial = initialL, step = stepL, extract = extractL }
        ShortcutFold{ initial = initialR, step = stepR, extract = extractR } =
          ShortcutFold
            { initial = Strict.shortcut2 initialL initialR
            , step = \(Strict.Tuple2 xL xR) a -> Strict.shortcut2
                (Strict.unlessDead (\x -> stepL x a) xL)
                (Strict.unlessDead (\x -> stepR x a) xR)
            , extract = \(Strict.Tuple2 xL xR) ->
                  extractL (shortcut xL) (extractR (shortcut xR))
            }

instance Semigroup b => Semigroup (ShortcutFold a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (ShortcutFold a b) where
    mempty = pure mempty
