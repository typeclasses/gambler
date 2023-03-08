module Fold.Shortcut.Type
  (
    ShortcutFold (..),
    Will (..), Vitality (..),
  )
  where

import Control.Applicative (Applicative, liftA2, pure, (<*>))
import Data.Functor (Functor, fmap)
import Data.Monoid (Monoid, mempty)
import Data.Semigroup (Semigroup, (<>))
import Strict (Will (..), Vitality (..))
import Data.Void (absurd)

import qualified Strict

{-| Processes inputs of type @a@, has the ability to halt midway
    through the stream, and results in a value of type @b@ -}
data ShortcutFold a b = forall x y. ShortcutFold
    { initial :: Vitality x y
    , step :: y -> a -> Vitality x y
    , extract :: Vitality x y -> b
    }

instance Functor (ShortcutFold a) where
    fmap f ShortcutFold{ step, initial, extract } =
        ShortcutFold{ initial, step, extract = \x -> f (extract x) }

instance Applicative (ShortcutFold a) where
    pure b = ShortcutFold
        { initial = Dead ()
        , step = absurd
        , extract = \e -> case e of { Dead () -> b }
        }

    (<*>)
        ShortcutFold{ initial = initialL, step = stepL, extract = extractL }
        ShortcutFold{ initial = initialR, step = stepR, extract = extractR } =
          ShortcutFold
            { initial = Strict.vitality2 initialL initialR
            , step = \(Strict.Tuple2 xL xR) a -> Strict.vitality2
                (Strict.unlessDead (\x -> stepL x a) xL)
                (Strict.unlessDead (\x -> stepR x a) xR)
            , extract = \e -> case e of { Dead x -> ex x; Alive _ x -> ex x }
            }
          where
            ex (Strict.Tuple2 xL xR) = (extractL xL) (extractR xR)

instance Semigroup b => Semigroup (ShortcutFold a b) where
    (<>) = liftA2 (<>)

instance Monoid b => Monoid (ShortcutFold a b) where
    mempty = pure mempty
