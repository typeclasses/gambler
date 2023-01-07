module Main (main) where

import Criterion.Main

import Data.Functor ((<$>))
import Control.Applicative (pure, (<*>))
import Data.Function ((.), ($))
import Data.Int (Int)
import Prelude ((+), Num)
import System.IO (IO)

import qualified Data.List as List
import qualified Prelude
import qualified Data.Foldable as Foldable

import qualified Fold.Pure
import qualified Fold.Effectful

main :: IO ()
main = defaultMain
  [ env (pure [1..10000 :: Int]) $ \ns ->
      bgroup "[1..10000 :: Int]"
        [ bgroup "sum" $ List.map ($ ns)
            [ bench "Fold.Pure.run sum" .
                whnf (Fold.Pure.run Fold.Pure.sum)
            , bench "Fold.Effectful.run (fold sum)" .
                whnfIO . Fold.Effectful.run (Fold.Effectful.fold Fold.Pure.sum)
            , bench "Prelude.sum" .
                whnf Prelude.sum
            , bench "Data.List.foldl' (+) 0" .
                whnf (List.foldl' (+) 0)
            ]
        , bgroup "length" $ List.map ($ ns)
            [ bench "Fold.Pure.run length" .
                whnf (Fold.Pure.run Fold.Pure.length)
            , bench "Fold.Effectful.run (generalize length)" .
                whnfIO . Fold.Effectful.run (Fold.Effectful.fold Fold.Pure.length)
            , bench "Prelude.length" .
                whnf Prelude.length
            ]
        , bgroup "sumAndLength" $ List.map ($ ns)
            [ bench "naive sumAndLength" .
                nf sumAndLength
            , bench "foldl' sumAndLength" .
                nf sumAndLength'
            , bench "strict pair sumAndLength" .
                nf sumAndLength_Pair
            , bench "foldl sumAndLength" .
                nf sumAndLength_foldl
            ]
        ]
  ]


sumAndLength :: Num a => [a] -> (a, Int)
sumAndLength xs = (Prelude.sum xs, Prelude.length xs)

sumAndLength' :: Num a => [a] -> (a, Int)
sumAndLength' xs = Foldable.foldl' step (0, 0) xs
  where
    step (x, y) n = (x + n, y + 1)

data Pair a b = Pair !a !b

sumAndLength_Pair :: Num a => [a] -> (a, Int)
sumAndLength_Pair xs = done (Foldable.foldl' step (Pair 0 0) xs)
  where
    step (Pair x y) n = Pair (x + n) (y + 1)

    done (Pair x y) = (x, y)

sumAndLength_foldl :: Num a => [a] -> (a, Int)
sumAndLength_foldl = Fold.Pure.run ((,) <$> Fold.Pure.sum <*> Fold.Pure.length)
