{-# LANGUAGE TemplateHaskell #-}

import           Test.DocTest
import           Test.QuickCheck
import           Test.Semiring

prop_plusCommInteger :: Integer -> Integer -> Property
prop_plusCommInteger = plusComm

return []

runTests :: IO Bool
runTests = $quickCheckAll

main :: IO Bool
main = do
  doctest [ "-isrc"
          , "src/Data/Semiring.hs"
          , "src/Data/Semiring/Numeric.hs"
          , "src/Test/Semiring.hs" ]
  runTests
