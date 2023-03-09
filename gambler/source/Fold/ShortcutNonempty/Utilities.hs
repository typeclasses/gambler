module Fold.ShortcutNonempty.Utilities where

import Fold.ShortcutNonempty.Type

import Fold.Shortcut.Type (ShortcutFold (ShortcutFold))
import Strict (willSave, willBoost, getVitality')

import qualified Strict
import qualified Fold.Shortcut.Type as Empty
import qualified Fold.Shortcut.Utilities as Empty
import qualified Fold.Shortcut.Run as Empty (run)
import qualified Fold.ShortcutNonempty.Conversion as Nonempty

motivate :: ShortcutNonemptyFold a b -> ShortcutNonemptyFold a b
motivate ShortcutNonemptyFold{ initial, step, extract } =
  ShortcutNonemptyFold
    { initial = \a -> willBoost (initial a)
    , step = \x a -> willBoost (step x a)
    , extract
    }

{-| Causes a shortcut fold to stop once it becomes ambivalent -}
demotivate :: ShortcutNonemptyFold a b -> ShortcutNonemptyFold a b
demotivate ShortcutNonemptyFold{ initial, step, extract } =
  ShortcutNonemptyFold
    { initial = \a -> willSave (initial a)
    , step = \x a -> willSave (step x a)
    , extract = \v -> case v of
        Dead e -> case e of
          Strict.Left x -> extract (Dead x)
          Strict.Right x -> extract (Alive Ambivalent x)
        Alive w x -> extract (Alive w x)
    }

{-| Allows to continue feeding a fold even after passing it to a function
    that closes it -}
duplicate :: ShortcutNonemptyFold a b -> ShortcutNonemptyFold a (ShortcutFold a b)
duplicate ShortcutNonemptyFold{ initial, step, extract } =
  ShortcutNonemptyFold
    { initial
    , step
    , extract = \v -> ShortcutFold{ Empty.initial = v, Empty.step, Empty.extract }
    }

withVitality :: ShortcutNonemptyFold a b -> ShortcutNonemptyFold a (Vitality' b)
withVitality ShortcutNonemptyFold{ initial, step, extract } =
  ShortcutNonemptyFold
    { initial
    , step
    , extract = \v -> let x = extract v in case v of
        Alive w _ -> Alive w x
        Dead _ -> Dead x
    }

{-| Convert a nonempty fold for a single item (@x@) into a
    nonempty fold for nonempty lists of items (@xs@) -}
repeatedly :: forall x xs result.
    (forall b. ShortcutNonemptyFold x b -> xs -> b)
        -- ^ A witness to the fact that @xs@ is a nonempty list of @x@
    -> ShortcutNonemptyFold x result
    -> ShortcutNonemptyFold xs result
repeatedly runXs foldX =
  ShortcutNonemptyFold
    { initial = \xs -> runXs (withVitality (duplicate foldX)) xs
    , step = \f xs -> runXs (withVitality (Nonempty.shortcutFold (Empty.duplicate f))) xs
    , extract = \f -> Empty.run (getVitality' f) []
    }

{-| Applies a function to each input before processing -}
premap :: (a -> b) -> ShortcutNonemptyFold b r -> ShortcutNonemptyFold a r
premap f ShortcutNonemptyFold{ initial, step, extract } =
    ShortcutNonemptyFold{ initial = \a -> initial (f a),
        step = \x a -> step x (f a), extract }
