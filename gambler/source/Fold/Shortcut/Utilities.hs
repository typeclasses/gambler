module Fold.Shortcut.Utilities where

import Fold.Shortcut.Type

import Strict (willSave)

import qualified Strict

demotivate :: ShortcutFold a b -> ShortcutFold a b
demotivate ShortcutFold{ initial, step, extractDead, extractLive } =
  ShortcutFold
    { initial = willSave initial
    , step = \x a -> willSave (step x a)
    , extractDead = \e -> case e of
          Strict.Left x -> extractDead x
          Strict.Right x -> extractLive x
    , extractLive = extractLive
    }
