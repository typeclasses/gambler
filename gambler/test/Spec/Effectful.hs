module Spec.Effectful where

import Fold.Effectful

import Test.Hspec

import Control.Applicative (pure, (<*>))
import Control.Monad ((<=<))
import Data.Foldable (traverse_)
import Data.Function (id, on, (.), (&))
import Data.Functor ((<$>))
import Data.Functor.Identity (Identity (Identity), runIdentity)
import Data.Monoid (mempty)
import Data.Semigroup (Sum (Sum), (<>))
import Prelude ((>), String, Integer, (+), (*))
import Data.Bool (Bool (..))

import qualified Data.List as List

spec :: SpecWith ()
spec = describe "EffectfulFold" do

    describe "drop" do
        it "run (drop n f) xs = run f (List.drop n xs)" do
            let xs = [10, 20, 30, 1, 2, 3] :: [Integer]
                f = sum :: EffectfulFold Identity Integer Integer
            [0 .. 8] & traverse_ @[] \n ->
                run (drop n f) xs `shouldBe` run f (List.genericDrop n xs)

    describe "effectMonoid" do
        it "If <> is commutative, \
            \effectMonoid (f <> g) = effectMonoid f <> effectMonoid g" do
            let xs = [(1, 3), (5, 4), (11, 23)] :: [(Integer, Integer)]
                f (x, _) = pure (Sum x)
                g (_, x) = pure (Sum x)
                (===) = shouldBe `on` \fo ->
                    runIdentity (run fo xs)
            effectMonoid (f <> g)
                === (effectMonoid f <> effectMonoid g)

        it "effectMonoid mempty = mempty" do
            let xs = ["one", "two", "three"] :: [String]
                (===) = shouldBe `on` \fo ->
                    runIdentity (run fo xs) :: Sum Integer
            effectMonoid mempty === mempty

    describe "premap" do
        it "premap pure = id" do
            let xs = [5, 13, 1] :: [Integer]
                (===) = shouldBe `on` \f ->
                    runIdentity (run (f sum) xs) :: Integer
            premap pure === id

        it "premap (f <=< g) = premap g . premap f" do
            let xs = [5, 13, 1] :: [Integer]
                f = Identity . (+ 13)
                g = Identity . (* 7)
                (===) = shouldBe `on` \h ->
                    runIdentity (run (h list) xs) :: [Integer]
            premap (f <=< g) === (premap g . premap f)

        it "premap k (pure r) = pure r" do
            let xs = [5, 13, 1] :: [Integer]
                k = Identity . (+ 13)
                r = "Hi." :: String
                (===) = shouldBe `on` \fo ->
                    runIdentity (run fo xs) :: String
            premap k (pure r) === pure r

        it "premap k (f <*> x) = premap k f <*> premap k x" do
            let xs = [1..10] :: [Integer]
                k = Identity . (+ 7)
                f = (+) <$> sum
                x = product
                (===) = shouldBe `on` \fo ->
                    runIdentity (run fo xs)
            premap k (f <*> x) === (premap k f <*> premap k x)

    describe "prefilter" do
        it "considers only inputs that match an effectful predicate" do
            let xs = [1..10] :: [Integer]
                p = (> 5)
                f = sum
            shouldBe @(Identity Integer)
                (run (prefilter (Identity . p) f) xs)
                (run f (List.filter p xs))

    describe "null" do
        it "True for []" do
            run null ([] :: [Integer]) `shouldBe` Identity True
        it "False for anything else" do
            run null ([1] :: [Integer]) `shouldBe` Identity False
            run null ([1,2] :: [Integer]) `shouldBe` Identity False
            run null ([1,2,3] :: [Integer]) `shouldBe` Identity False
