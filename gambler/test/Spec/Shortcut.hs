module Spec.Shortcut where

import Fold.Shortcut

import Test.Hspec

import Control.Applicative (liftA2, pure)
import Prelude (Integer, undefined)
import Data.Maybe (Maybe (Just, Nothing))
import Data.Char (Char)
import Numeric.Natural (Natural)
import Data.List ((++))

import qualified Data.Char as Char

spec :: SpecWith ()
spec = describe "ShortcutFold" do

    it "Applicative" do
        let x = do
                  a <- length
                  b <- find Char.isLetter
                  c <- elementIndex 'x'
                  d <- elementIndex 'y'
                  pure (a, b, c, d)
        run x ("1234xyz" ++ undefined) `shouldBe` (6, Just 'x', Just 4, Just 5)

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
