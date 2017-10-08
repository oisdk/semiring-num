module Main (main) where

import           Criterion.Main

import           System.Random

import           Control.Monad

import           Data.Semiring

threeInts :: IO (Int,Int,Int)
threeInts = (,,) <$> randomIO <*> randomIO <*> randomIO

int :: IO Int
int = randomIO

sumAtSize :: Int -> Benchmark
sumAtSize n =
    env (replicateM n threeInts) $
    \xs ->
         bgroup (show n) [bench "add" $ nf add xs]

prodAtSize :: Int -> Int -> Benchmark
prodAtSize n m =
    env ((,) <$> replicateM n int <*> replicateM m int) $
    \xs ->
         bench "prod-list" (nf (uncurry (<.>)) xs)

main :: IO ()
main = defaultMain [prodAtSize 2000 1000, sumAtSize 10000]
