module Main (main) where

import           Criterion.Main

import           System.Random

import           Control.Monad

import           Data.Semiring

import qualified Data.Vector as Vector

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

prodAtSizeVec :: Int -> Int -> Benchmark
prodAtSizeVec n m =
    env ((,) <$> Vector.replicateM n int <*> Vector.replicateM m int) $
    \xs ->
         bgroup
             "vec"
             [ bench (show n ++ "<.>" ++ show m) (nf (uncurry (<.>)) xs)
             , bench (show m ++ "<.>" ++ show n) (nf (uncurry (flip (<.>))) xs)
             , bench (show n ++ "<+>" ++ show m) (nf (uncurry (<+>)) xs)
             , bench (show m ++ "<+>" ++ show n) (nf (uncurry (flip (<+>))) xs)
             ]

main :: IO ()
main =
    defaultMain
        [ prodAtSizeVec 4000 2000
        , prodAtSizeVec 4000 2000]



-- prodAtSize 2000 1000, sumAtSize 10000]
