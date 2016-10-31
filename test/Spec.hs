module Main (main) where

import           Data.Proxy
import           Test.DocTest
import           Test.QuickCheck
import           Test.Semiring
import           Data.Semiring
import Data.Set (Set)

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
  doctest [ "-isrc"
          , "src/Data/Semiring.hs"
          , "src/Data/Semiring/Numeric.hs"
          , "src/Test/Semiring.hs" ]
