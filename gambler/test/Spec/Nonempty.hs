module Spec.Nonempty where

import Fold.Nonempty

import Test.Hspec

import Control.Applicative (liftA2)
import Data.Function ((&), flip)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (Maybe (..))
import Positive (Positive)
import Prelude (String, Integer, (+), odd)

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
            let a :: NonEmpty Integer
                b :: [Integer]
                a = [1..3]
                b = [4..6]
            (sum & duplicate & flip run a & flip Pure.run b)
                `shouldBe` (List.sum a + List.sum b)

    describe "repeatedly" do
        let a, b, c :: NonEmpty Integer
            a = [2, 4]
            b = [10, 12, 5, 7, 8]
            c = [4, 5, 6]
        it "can fold a list of lists" do
            run (repeatedly run maximum) [a, b, c] `shouldBe` 12
        it "works in Applicative combination with another fold" do
            let f = liftA2 (,) (repeatedly run length) length
            run f [a, b, c] `shouldBe` (10, 3)
        it "works in Applicative combination with another 'repeatedly'" do
            let f = liftA2 (,) (repeatedly run (find odd)) (repeatedly run maximum)
            run f [a, b, c] `shouldBe` (Just 5, 12)
        it "works on an Applicative combination" do
            let f = liftA2 (,) (find odd) maximum
            run (repeatedly run f) [a, b, c] `shouldBe` (Just 5, 12)
