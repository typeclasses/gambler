module Spec.ShortcutNonempty where

import Fold.ShortcutNonempty

import Test.Hspec

import Prelude (Integer, undefined)
import Data.List.NonEmpty (NonEmpty ((:|)))

spec :: SpecWith ()
spec = describe "ShortcutNonemptyFold" do

    describe "first" do
        it "gets the first item" do
            shouldBe @Integer (run first [5, 4, 3]) 5
        it "is lazy" do
            shouldBe @Integer (run first (5 :| undefined)) 5
