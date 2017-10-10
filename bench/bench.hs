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

atSizeList :: Int -> Int -> Benchmark
atSizeList n m =
    env ((,) <$> replicateM n int <*> replicateM m int) $
    \xs ->
         bgroup
             (show (n, m))
             [ bench (show n ++ "<.>" ++ show m) (nf (uncurry (<.>)) xs)
             , bench (show m ++ "<.>" ++ show n) (nf (uncurry (flip (<.>))) xs)
             , bench (show n ++ "<+>" ++ show m) (nf (uncurry (<+>)) xs)
             , bench (show m ++ "<+>" ++ show n) (nf (uncurry (flip (<+>))) xs)]

starList :: Int -> Benchmark
starList n = env (replicateM n (pure ())) $ \xs -> bench (show n) (nf (take n . star) xs)

atSizeVec :: Int -> Int -> Benchmark
atSizeVec n m =
    env ((,) <$> Vector.replicateM n int <*> Vector.replicateM m int) $
    \xs ->
         bgroup
             (show (n, m))
             [ bench (show n ++ "<.>" ++ show m) (nf (uncurry (<.>)) xs)
             , bench (show m ++ "<.>" ++ show n) (nf (uncurry (flip (<.>))) xs)
             , bench (show n ++ "<+>" ++ show m) (nf (uncurry (<+>)) xs)
             , bench (show m ++ "<+>" ++ show n) (nf (uncurry (flip (<+>))) xs)]

main :: IO ()
main =
    defaultMain
        [ bgroup "list star" [starList 2000]
        , bgroup "vec" [atSizeVec 4000 2000, atSizeVec 4000 2000]
        , bgroup "list" [atSizeList 400 200, atSizeList 4000 2000]
        , bgroup "add" [sumAtSize 100, sumAtSize 1000]]
