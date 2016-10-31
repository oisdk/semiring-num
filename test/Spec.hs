{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Data.Proxy            (Proxy (..))
import           Data.Semiring         (Add, Mul)
import           Data.Semiring.Numeric
import           Data.Set              (Set)
import           Test.DocTest
import           Test.QuickCheck
import           Test.Semiring

main :: IO ()
main = do
  quickCheck (semiringLaws (Proxy :: Proxy Integer))
  quickCheck (semiringLaws (Proxy :: Proxy Bool))
  quickCheck (semiringLaws (Proxy :: Proxy (Add Integer)))
  quickCheck (semiringLaws (Proxy :: Proxy (Mul Integer)))
  quickCheck (semiringLaws (Proxy :: Proxy (Set Ordering)))
  quickCheck (semiringLaws (Proxy :: Proxy (Integer,Integer)))
  quickCheck (semiringLaws (Proxy :: Proxy (Integer,Integer,Integer)))
  quickCheck (semiringLaws (Proxy :: Proxy (Integer,Integer,Integer,Integer)))
  quickCheck (semiringLaws (Proxy :: Proxy (Integer,Integer,Integer,Integer,Integer)))
  quickCheck (semiringLaws (Proxy :: Proxy (Division Integer)))
  quickCheck (semiringLaws (Proxy :: Proxy (Bottleneck Int)))
  quickCheck (semiringLaws (Proxy :: Proxy (Łukasiewicz Integer)))
  quickCheck (semiringLaws (Proxy :: Proxy (Viterbi Integer)))
  doctest [ "-isrc"
          , "src/Data/Semiring.hs"
          , "src/Data/Semiring/Numeric.hs"
          , "src/Test/Semiring.hs" ]
