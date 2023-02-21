module Spec.ShortcutNonempty where

import Fold.ShortcutNonempty

import Test.Hspec

import Control.Applicative (pure, (<$>), (<*>))
import Prelude (Integer, undefined)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (Maybe (Just))
import Data.Bool (Bool (..))
import Data.List ((++))

import qualified Data.Char as Char

spec :: SpecWith ()
spec = describe "ShortcutNonemptyFold" do

    it "Applicative" do
        let x = do
                  a <- length
                  b <- find Char.isLetter
                  c <- elementIndex 'x'
                  d <- elementIndex 'y'
                  e <- last
                  pure (a, b, c, d, e)
        run x ('1' :| "234xyz" ++ undefined) `shouldBe` (6, Just 'x', Just 4, Just 5, 'y')

    describe "first" do
        it "gets the first item" do
            shouldBe @Integer (run first [5, 4, 3]) 5
        it "is lazy" do
            shouldBe @Integer (run first (5 :| undefined)) 5
        it "is tenacious" do
            shouldBe ((run ((,) <$> length <*> first)) ('a' :| "bc")) (1, 'a')

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
