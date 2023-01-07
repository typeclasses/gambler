module Spec.Nonempty where

import Fold.Nonempty

import Test.Hspec

import Data.List.NonEmpty (NonEmpty ((:|)))
import Prelude (String, Integer)

spec :: SpecWith ()
spec = describe "NonemptyFold" do

    describe "semigroup" do
        it "folds all inputs using (<>)" do
            let xs = "Hello" :| " " : "world" : [] :: NonEmpty String
            run semigroup xs `shouldBe` "Hello world"

    describe "minimum" do
        it "produces the least input" do
            let xs = [5, 3, 7, 2, 9, 4] :: NonEmpty Integer
            run minimum xs `shouldBe` 2

    describe "maximum" do
        it "produces the greatest input" do
            let xs = [5, 3, 7, 2, 9, 4] :: NonEmpty Integer
            run maximum xs `shouldBe` 9

    describe "listing functions" do
        let xs = [1 .. 4] :: NonEmpty Integer

        describe "list" do
            it "gets all inputs" do
                run list xs `shouldBe` xs
        describe "reverseList" do
            it "gets all inputs in reverse" do
                run reverseList xs `shouldBe` [4, 3, 2, 1]
