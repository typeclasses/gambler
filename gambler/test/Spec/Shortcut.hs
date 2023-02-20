module Spec.Shortcut where

import Fold.Shortcut

import Test.Hspec

import Control.Applicative (pure)
import Prelude (Integer, undefined)
import Data.Maybe (Maybe (Just, Nothing))
import Data.List ((++))
import Data.Bool (Bool (..))

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
