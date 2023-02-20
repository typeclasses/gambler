module Fold.ShortcutNonempty.Utilities where

import Fold.ShortcutNonempty.Type

import Strict (willSave)

import qualified Strict

demotivate :: ShortcutNonemptyFold a b -> ShortcutNonemptyFold a b
demotivate ShortcutNonemptyFold{ initial, step, extractDead, extractLive } =
  ShortcutNonemptyFold
    { initial = \a -> willSave (initial a)
    , step = \x a -> willSave (step x a)
    , extractDead = \e -> case e of
          Strict.Left x -> extractDead x
          Strict.Right x -> extractLive x
    , extractLive = extractLive
    }
