module Fold.Shortcut.Utilities where

import Fold.Shortcut.Type

import Fold.Shortcut.Run (run)
import Strict (willSave, willBoost, Vitality', getVitality')

import qualified Strict

motivate :: ShortcutFold a b -> ShortcutFold a b
motivate ShortcutFold{ initial, step, extract } =
  ShortcutFold
    { initial = willBoost initial
    , step = \x a -> willBoost (step x a)
    , extract
    }

{-| Causes a shortcut fold to stop once it becomes ambivalent -}
demotivate :: ShortcutFold a b -> ShortcutFold a b
demotivate ShortcutFold{ initial, step, extract } =
  ShortcutFold
    { initial = willSave initial
    , step = \x a -> willSave (step x a)
    , extract = \v -> case v of
        Dead e -> case e of
          Strict.Left x -> extract (Dead x)
          Strict.Right x -> extract (Alive Ambivalent x)
        Alive w x -> extract (Alive w x)
    }

{-| Allows to continue feeding a fold even after passing it to a function
    that closes it -}
duplicate :: ShortcutFold a b -> ShortcutFold a (ShortcutFold a b)
duplicate ShortcutFold{ initial, step, extract } =
  ShortcutFold
    { initial
    , step
    , extract = \v -> ShortcutFold{ initial = v, step, extract }
    }

withVitality :: ShortcutFold a b -> ShortcutFold a (Vitality' b)
withVitality ShortcutFold{ initial, step, extract } =
  ShortcutFold
    { initial
    , step
    , extract = \v -> let x = extract v in case v of
        Alive w _ -> Alive w x
        Dead _ -> Dead x
    }

{-| Convert a fold for a single item (@x@) into a fold for lists
    of items (@xs@) -}
repeatedly :: forall x xs result.
    (forall b. ShortcutFold x b -> xs -> b)
        -- ^ A witness to the fact that @xs@ is a list of @x@
    -> ShortcutFold x result
    -> ShortcutFold xs result
repeatedly runXs foldX =
  ShortcutFold
    { initial = run (withVitality (duplicate foldX)) []
    , step = \f xs -> runXs (withVitality (duplicate f)) xs
    , extract = \f -> run (getVitality' f) []
    }

{-| Applies a function to each input before processing -}
premap :: (a -> b) -> ShortcutFold b r -> ShortcutFold a r
premap f ShortcutFold{ initial, step, extract } =
    ShortcutFold{ initial, step = \x a -> step x (f a), extract }
