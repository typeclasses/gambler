module Main (main) where

import Prelude

import Test.Hspec

import qualified Spec.Pure
import qualified Spec.Nonempty
import qualified Spec.Effectful
import qualified Spec.Shortcut
import qualified Spec.ShortcutNonempty

main :: IO ()
main = hspec do
    Spec.Pure.spec
    Spec.Nonempty.spec
    Spec.Effectful.spec
    Spec.Shortcut.spec
    Spec.ShortcutNonempty.spec
