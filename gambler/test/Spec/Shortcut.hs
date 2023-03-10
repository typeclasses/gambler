module Spec.Shortcut where

import Fold.Shortcut

import Test.Hspec

import Control.Applicative (pure, (<$>), (<*>), liftA2)
import Data.Bool (Bool (..))
import Data.Function ((&), flip)
import Data.List ((++))
import Data.Maybe (Maybe (Just, Nothing))
import Data.Ord (Ord (..))
import Prelude (Integer, undefined, odd)

import qualified Data.Char as Char

spec :: SpecWith ()
spec = describe "ShortcutFold" do

    it "Applicative" do
        let x = do
                  a <- length
                  b <- find Char.isLetter
                  c <- elementIndex 'x'
                  d <- elementIndex 'y'
                  e <- last
                  pure (a, b, c, d, e)
        run x ("1234xyz" ++ undefined) `shouldBe` (6, Just 'x', Just 4, Just 5, Just 'y')

    describe "first" do
        it "gets the first item" do
            shouldBe @(Maybe Integer) (run first [5, 4, 3]) (Just 5)
        it "is lazy" do
            shouldBe @(Maybe Integer) (run first (5 : undefined)) (Just 5)
        it "returns Nothing for []" do
            shouldBe @(Maybe Integer) (run first []) Nothing
        it "is tenacious" do
            shouldBe ((run ((,) <$> length <*> first)) "abc") (1, Just 'a')

    describe "endpoint functions" do
        describe "first" do
            it "gets the first item" do
                run first ([5, 4, 3] :: [Integer]) `shouldBe` Just 5
            it "returns Nothing with no input" do
                run first ([] :: [Integer]) `shouldBe` Nothing
            it "is lazy" do
                run first (5 : undefined :: [Integer]) `shouldBe` Just 5

    describe "null" do
        it "True for []" do
            run null ([] :: [Integer]) `shouldBe` True
        it "False for anything else" do
            run null ([1] :: [Integer]) `shouldBe` False
            run null ([1,2] :: [Integer]) `shouldBe` False
            run null ([1,2,3] :: [Integer]) `shouldBe` False

    describe "and" do
        it "True for []" do
            run and [] `shouldBe` True
        it "True for [True, ...]" do
            run and [True] `shouldBe` True
            run and [True,True] `shouldBe` True
            run and [True,True,True] `shouldBe` True
        it "False for anything else" do
            run and [False] `shouldBe` False
            run and [False,True] `shouldBe` False
            run and [True,False] `shouldBe` False
        it "is lazy" do
            run and (False : undefined) `shouldBe` False

    describe "or" do
        it "False for []" do
            run or [] `shouldBe` False
        it "False for [False, ...]" do
            run or [False] `shouldBe` False
            run or [False,False] `shouldBe` False
            run or [False,False,False] `shouldBe` False
        it "True for anything else" do
            run or [True] `shouldBe` True
            run or [False,True] `shouldBe` True
            run or [True,False] `shouldBe` True
        it "is lazy" do
            run or (True : undefined) `shouldBe` True

    describe "duplicate" do
        it "lets a fold run in two phases" do
            let a, b, c :: [Integer]
                a = [1,2,3]
                b = [4,7,12,15]
                c = a ++ b
                f = find (>= 10)
            (f & duplicate & flip run a & flip run b) `shouldBe` (run f c)
        it "preserves laziness" do
            let a, b :: [Integer]
                a = 1 : 15 : undefined
                b = undefined
                f = find (>= 10)
            (f & duplicate & flip run a & flip run b) `shouldBe` Just 15

    describe "repeatedly" do
        it "can find an item in a list of lists" do
            let a, b, c :: [Integer]
                a = [2, 8, 6]
                b = [12, 4, 5, 16]
                c = [4, 18, 20]
            run (repeatedly run (find odd)) [a, b, c]
                `shouldBe` Just 5
        it "preserves laziness" do
            let a, b, c :: [Integer]
                a = [2, 8, 6]
                b = 12 : 4 : 5 : undefined
                c = undefined
            run (repeatedly run (find odd)) [a, b, c]
                `shouldBe` Just 5
        it "works within an Applicative combination with another 'repeatedly'" do
            let a, b, c :: [Integer]
                a = [2, 8, 6]
                b = 12 : 4 : 5 : undefined
                c = undefined
                f = liftA2 (,) (repeatedly run (find odd)) (repeatedly run (find (> 5)))
            run f [a, b, c]
                `shouldBe` (Just 5, Just 8)
        it "works on an Applicative combination of 'repeatedly's" do
            let a, b, c :: [Integer]
                a = [2, 8, 6]
                b = 12 : 4 : 5 : undefined
                c = undefined
                f = liftA2 (,) (find odd) (find (> 5))
            run (repeatedly run f) [a, b, c]
                `shouldBe` (Just 5, Just 8)
        it "works within an Applicative combination with something ambivalent" do
            let a, b, c :: [Integer]
                a = [2, 8, 6]
                b = 12 : 4 : 5 : undefined
                c = undefined
                f = liftA2 (,) (find odd) length
            run (repeatedly run f) [a, b, c]
                `shouldBe` (Just 5, 6)
