module Spec.ShortcutNonempty where

import Fold.ShortcutNonempty

import Test.Hspec

import Prelude (Integer, undefined)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Bool (Bool (..))

spec :: SpecWith ()
spec = describe "ShortcutNonemptyFold" do

    describe "first" do
        it "gets the first item" do
            shouldBe @Integer (run first [5, 4, 3]) 5
        it "is lazy" do
            shouldBe @Integer (run first (5 :| undefined)) 5

    describe "and" do
        it "True for [True, ...]" do
            run and [True] `shouldBe` True
            run and [True,True] `shouldBe` True
            run and [True,True,True] `shouldBe` True
        it "False for anything else" do
            run and [False] `shouldBe` False
            run and [False,True] `shouldBe` False
            run and [True,False] `shouldBe` False
        it "is lazy" do
            run and (False :| undefined) `shouldBe` False

    describe "or" do
        it "False for [False, ...]" do
            run or [False] `shouldBe` False
            run or [False,False] `shouldBe` False
            run or [False,False,False] `shouldBe` False
        it "True for anything else" do
            run or [True] `shouldBe` True
            run or [False,True] `shouldBe` True
            run or [True,False] `shouldBe` True
        it "is lazy" do
            run or (True :| undefined) `shouldBe` True
