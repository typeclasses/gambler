module Spec.Pure where

import Fold.Pure

import Test.Hspec

import Control.Applicative (pure, (<*>))
import Data.Foldable (traverse_)
import Data.Function (id, on, (.), (&))
import Data.Functor ((<$>))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Monoid (mempty)
import Data.Semigroup (Sum (Sum))
import Prelude ((>), String, Integer, (+), (*))

import qualified Data.Foldable as Foldable
import qualified Data.List as List

spec :: SpecWith ()
spec = describe "Fold" do

    describe "scanning functions" do
        let xs = [1 .. 5] :: [Integer]

        describe "scan" do
            it "gives all the intermediate states" do
                scan length xs `shouldBe` [0 .. 5]
        describe "prescan" do
            it "excludes the final state" do
                prescan length xs `shouldBe` [0 .. 4]
        describe "postscan" do
            it "excludes the initial state" do
                postscan length xs `shouldBe` [1 .. 5]

    describe "premap" do
        let xs = [1 .. 10] :: [Integer]

        it "applies f to each input" do
            let f = Sum
                fold = monoid
                z = Foldable.foldMap Sum xs
            run (premap f fold) xs `shouldBe` z
            run fold (List.map f xs)    `shouldBe` z
        it "premap id = id" do
            let fold = sum
                (===) = shouldBe `on` \f -> run (f fold) xs
            premap id === id
        it "premap (f . g) = premap g . premap f" do
            let fold = sum
                f = (+ 1)
                g = (* 2)
                (===) = shouldBe `on` \r -> run (r fold) xs
            premap (f . g) === (premap g . premap f)
        it "premap k (pure r) = pure r" do
            let r = 5 :: Integer
                k = (+ 1)
                (===) = shouldBe `on` \fold -> run fold xs
            premap k (pure r) === pure r
        it "premap k (f <*> x) = premap k f <*> premap k x" do
            let k = (+ 1)
                f = (+) <$> product
                x = sum
                (===) = shouldBe `on` \fold -> run fold xs
            premap k (f <*> x) === (premap k f <*> premap k x)

    describe "prefilter" do
        it "run (prefilter p f) xs = run f (List.filter p xs)" do
            let xs = [1 .. 10] :: [Integer]
                p = (> 5)
                f = sum
            run (prefilter p f) xs `shouldBe` run f (List.filter p xs)

    describe "predropWhile" do
        it "run (predropWhile p f) xs = run f (List.dropWhile p xs)" do
            let xs = [10, 9, 5, 9] :: [Integer]
                fo = sum
                p = (> 5)
            run (predropWhile p fo) xs `shouldBe` run fo (List.dropWhile p xs)

    describe "drop" do
        it "run (drop n f) xs = run f (List.drop n xs)" do
            let xs = [10, 20, 30, 1, 2, 3] :: [Integer]
                f = sum :: Fold Integer Integer
            [0 .. 8] & traverse_ @[] \n ->
                run (drop n f) xs `shouldBe` run f (List.genericDrop n xs)

    describe "sum" do
        it "computes the sum of all inputs" do
            let xs = [1 .. 10] :: [Integer]
            run sum xs `shouldBe` 55

    describe "product" do
        it "computes the product of all inputs" do
            let xs = [1 .. 5] :: [Integer]
            run product xs `shouldBe` 120

    describe "monoid" do
        it "folds all inputs using (<>) and mempty" do
            let xs = ["Hello", " ", "world"] :: [String]
            run monoid xs `shouldBe` "Hello world"
        it "returns mempty when there are no inputs" do
            run monoid ([] :: [String]) `shouldBe` mempty

    describe "index" do
        let xs = [4, 5, 6] :: [Integer]
        it "0" do run (index 0) xs `shouldBe` Just 4
        it "1" do run (index 1) xs `shouldBe` Just 5
        it "2" do run (index 2) xs `shouldBe` Just 6
        it "3" do run (index 3) xs `shouldBe` Nothing

    describe "listing functions" do
        let xs = [1 .. 4] :: [Integer]

        describe "list" do
            it "gets all inputs" do run list xs `shouldBe` xs
        describe "reverseList" do
            it "gets all inputs in reverse" do
                run reverseList xs `shouldBe` [4, 3, 2, 1]

    describe "endpoint functions" do
        describe "first" do
            it "gets the first item" do
                run first ([5, 4, 3] :: [Integer]) `shouldBe` Just 5
            it "returns Nothing with no input" do
                run first ([] :: [Integer]) `shouldBe` Nothing
        describe "last" do
            it "gets the last item" do
                run last ([5, 4, 3] :: [Integer]) `shouldBe` Just 3
            it "returns Nothing with no input" do
                run last ([] :: [Integer]) `shouldBe` Nothing
