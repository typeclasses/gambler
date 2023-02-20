module Spec.Shortcut where

import Fold.Shortcut

import Test.Hspec

import Control.Applicative (pure)
import Prelude (Integer, undefined)
import Data.Maybe (Maybe (Just, Nothing))

spec :: SpecWith ()
spec = describe "ShortcutFold" do

    describe "endpoint functions" do
        describe "first" do
            it "gets the first item" do
                run first ([5, 4, 3] :: [Integer]) `shouldBe` Just 5
            it "returns Nothing with no input" do
                run first ([] :: [Integer]) `shouldBe` Nothing
            it "is lazy" do
                run first (5 : undefined :: [Integer]) `shouldBe` Just 5
        -- describe "last" do
        --     it "gets the last item" do
        --         run last ([5, 4, 3] :: [Integer]) `shouldBe` Just 3
        --     it "returns Nothing with no input" do
        --         run last ([] :: [Integer]) `shouldBe` Nothing
