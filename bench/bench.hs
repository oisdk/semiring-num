module Main (main) where

import           Criterion.Main

import           System.Random

import           Control.Monad

import           Data.Semiring
import           Data.Semiring.Vector

import qualified Data.Vector.Unboxed as Vector
import Data.Int

threeInts :: IO (Int,Int,Int)
threeInts = (,,) <$> randomIO <*> randomIO <*> randomIO

int :: IO Int8
int = randomIO

flt :: IO Float
flt = randomIO

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
             , bench (show n ++ "<+>" ++ show m) (nf (uncurry (<+>)) xs)
             , bench (show n ++ "p+" ++ show m) (nf (uncurry addInt8s) xs)
             , bench (show n ++ "p*" ++ show m) (nf (uncurry convInt8s) xs)
             ]

main :: IO ()
main =
    defaultMain
        -- [ bgroup "list star" [starList 2000]
        [ bgroup "vec" [atSizeVec 4096 64, atSizeVec 4000 2000]]
        -- , bgroup "list" [atSizeList 400 200, atSizeList 4000 2000]
        -- , bgroup "add" [sumAtSize 100, sumAtSize 1000]]
