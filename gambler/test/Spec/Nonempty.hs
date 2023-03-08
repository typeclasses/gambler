module Spec.Nonempty where

import Fold.Nonempty

import Test.Hspec

import Data.Function ((&), flip)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Positive (Positive)
import Prelude (String, Integer)

import qualified Data.List as List
import qualified Fold.Pure as Pure

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

    describe "sum/product" do
        it "sum works with Positive" do
            run sum [1,2,5] `shouldBe` (8 :: Positive)
        it "product works with Positive" do
            run product [1,2,5] `shouldBe` (10 :: Positive)

    describe "duplicate" do
        it "lets a fold run in two phases" do
            let a, c :: NonEmpty Integer
                b :: [Integer]
                a = [1..3]
                b = [4..6]
                c = [1..6]
            (sum & duplicate & flip run a & flip Pure.run b) `shouldBe` (List.sum c)
