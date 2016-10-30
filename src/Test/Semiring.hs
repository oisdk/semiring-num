{-|
Module: Test.Semiring
Description: Some QuickCheck properties for Semirings
License: MIT
Maintainer: mail@doisinkidney.com
Stability: experimental
-}

module Test.Semiring where

import Data.Semiring
import Test.QuickCheck

-- | Plus is associative.
plusAssoc :: (Eq a, Semiring a, Show a) => a -> a -> a -> Property
plusAssoc x y z = counterexample s res where
  res = lp == rp
  l = x <+> y
  r = y <+> z
  lp = l <+> z
  rp = x <+> r
  s = unlines
    [ "Testing associativity of plus."
    , "Law: (x <+> y) <+> z = x <+> (y <+> z)"
    , "x <+> y = " ++ show l
    , "y <+> z = " ++ show r
    , "(x <+> y) <+> z = " ++ show lp
    , "x <+> (y <+> z) = " ++ show rp]

-- | Multiplication is associative.
mulAssoc :: (Eq a, Semiring a, Show a) => a -> a -> a -> Property
mulAssoc x y z = counterexample s (lp == rp) where
  l = x <.> y
  r = y <.> z
  lp = l <.> z
  rp = x <.> r
  s = unlines
    [ "Testing associativity of <.>."
    , "Law: (x <.> y) <.> z = x <.> (y <.> z)"
    , "x <.> y = " ++ show l
    , "y <.> z = " ++ show r
    , "(x <.> y) <.> z = " ++ show lp
    , "x <.> (y <.> z) = " ++ show rp]

-- | Plus is commutative.
plusComm :: (Eq a, Semiring a, Show a) => a -> a -> Property
plusComm x y = counterexample s (l == r) where
  l = x <+> y
  r = y <+> x
  s = unlines
    [ "Testing commutativity of <+>."
    , "Law: x <+> y = y <+> x"
    , "x <+> y = " ++ show l
    , "y <+> x = " ++ show r ]

-- | Multiplication distributes left.
mulDistribL :: (Eq a, Semiring a, Show a) => a -> a -> a -> Property
mulDistribL x y z = counterexample s (l == r) where
  l = x <.> (y <+> z)
  r = x <.> y <+> x <.> z
  s = unlines
    [ "Testing left distributivity of <.> over <+>."
    , "Law: x <.> (y <+> z) = x <.> y <+> x <.> z"
    , "x <.> (y <+> z) = " ++ show l
    , "x <.> y <+> x <.> z  = " ++ show r ]

-- | Multiplication distributes right.
mulDistribR :: (Eq a, Semiring a, Show a) => a -> a -> a -> Property
mulDistribR x y z = counterexample s (l == r) where
  l = (x <+> y) <.> z
  r = x <.> z <+> y <.> z
  s = unlines
    [ "Testing right distributivity of <.> over <+>."
    , "Law: (x <+> y) <.> z = x <.> z <+> y <.> z"
    , "(x <+> y) <.> z = " ++ show l
    , "x <.> z <+> y <.> z = " ++ show r ]

-- | Additive identity
plusId :: (Eq a, Semiring a) => a -> Bool
plusId x = x <+> zero == x && zero <+> x == x

-- | Multiplicative identity
mulId :: (Eq a, Semiring a) => a -> Bool
mulId x = x <.> one == x && one <.> x == x
