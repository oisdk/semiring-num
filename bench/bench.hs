module Main (main) where

import           Criterion.Main

import           System.Random

import           Control.Monad

import           Data.Semiring

threeInts :: IO (Int,Int,Int)
threeInts = (,,) <$> randomIO <*> randomIO <*> randomIO

sumAtSize :: Int -> Benchmark
sumAtSize n =
    env (replicateM n threeInts) $
    \xs ->
         bgroup (show n) [bench "add" $ nf add xs]
main :: IO ()
main = defaultMain [sumAtSize 10000]
