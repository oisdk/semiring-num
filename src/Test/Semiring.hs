{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module: Test.Semiring
Description: Some QuickCheck properties for Semirings
License: MIT
Maintainer: mail@doisinkidney.com
Stability: experimental
-}

module Test.Semiring
  ( plusAssoc
  , mulAssoc
  , plusComm
  , mulDistribL
  , mulDistribR
  , plusId
  , mulId
  , annihilate
  , unaryLaws
  , binaryLaws
  , ternaryLaws
  ) where

import           Data.Semiring   (Semiring (..))

-- | Plus is associative.
plusAssoc :: (Eq a, Semiring a, Show a) => a -> a -> a -> Either String String
plusAssoc x y z = if res then Right s else Left s where
  res = lp == rp
  l = x <+> y
  r = y <+> z
  lp = l <+> z
  rp = x <+> r
  s = unlines
    [ "<+> is " ++ (if res then "" else "not ") ++ "associative."
    , "    Law:"
    , "        (x <+> y) <+> z = x <+> (y <+> z)"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z
    , "    x <+> y = " ++ show l
    , "    y <+> z = " ++ show r
    , "    (x <+> y) <+> z = " ++ show lp
    , "    x <+> (y <+> z) = " ++ show rp ]

-- | Multiplication is associative.
mulAssoc :: (Eq a, Semiring a, Show a) => a -> a -> a -> Either String String
mulAssoc x y z = if res then Right s else Left s  where
  res = lp == rp
  l = x <.> y
  r = y <.> z
  lp = l <.> z
  rp = x <.> r
  s = unlines
    [ "<+> is " ++ (if res then "" else "not ") ++ "associative."
    , "    Law:"
    , "        (x <.> y) <.> z = x <.> (y <.> z)"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z
    , "    x <.> y = " ++ show l
    , "    y <.> z = " ++ show r
    , "    (x <.> y) <.> z = " ++ show lp
    , "    x <.> (y <.> z) = " ++ show rp]

-- | Plus is commutative.
plusComm :: (Eq a, Semiring a, Show a) => a -> a -> Either String String
plusComm x y = if res then Right s else Left s where
  res = l == r
  l = x <+> y
  r = y <+> x
  s = unlines
    [ "<+> is " ++ (if res then "" else "not ") ++ "commutative."
    , "    Law:"
    , "        x <+> y = y <+> x"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    x <+> y = " ++ show l
    , "    y <+> x = " ++ show r ]

-- | Multiplication distributes left.
mulDistribL :: (Eq a, Semiring a, Show a) => a -> a -> a -> Either String String
mulDistribL x y z = if res then Right s else Left s where
  res = l == r 
  l = x <.> (y <+> z)
  r = x <.> y <+> x <.> z
  s = unlines
    [ "<.> does " ++ (if res then "" else "not ") ++ "distribute left over <+>."
    , "    Law:"
    , "        x <.> (y <+> z) = x <.> y <+> x <.> z"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z
    , "    x <.> (y <+> z) = " ++ show l
    , "    x <.> y <+> x <.> z  = " ++ show r ]

-- | Multiplication distributes right.
mulDistribR :: (Eq a, Semiring a, Show a) => a -> a -> a -> Either String String
mulDistribR x y z = if res then Right s else Left s where
  res = l == r
  l = (x <+> y) <.> z
  r = x <.> z <+> y <.> z
  s = unlines
    [ "<.> does " ++ (if res then "" else "not ") ++ "distribute left over <+>."
    , "    Law:"
    , "        (x <+> y) <.> z = x <.> z <+> y <.> z"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z
    , "    (x <+> y) <.> z = " ++ show l
    , "    x <.> z <+> y <.> z = " ++ show r ]

-- | Additive identity.
plusId :: (Eq a, Semiring a, Show a) => a -> Either String String
plusId (x :: a) = if res then Right s else Left s where
  res = l == x && r ==x
  l = x <+> zero
  r = zero <+> x
  s = unlines
    [ "zero is" ++ (if res then "" else " not") ++ " the identity of <+>."
    , "    Law:"
    , "        x <+> zero = zero <+> x = x"
    , "    x = " ++ show x
    , "    zero = " ++ show (zero :: a)
    , "    x <+> zero = " ++ show l
    , "    zero <+> x = " ++ show r ]

-- | Multiplicative identity.
mulId :: (Eq a, Semiring a, Show a) => a -> Either String String
mulId (x :: a) = if res then Right s else Left s where
  res = l == x && r == x
  l = x <.> one
  r = one <.> x
  s = unlines
    [ "one is" ++ (if res then "" else " not") ++ " the identity of <+>."
    , "    Law:"
    , "        x <.> one = one <.> x = x"
    , "    x = " ++ show x
    , "    one = " ++ show (one :: a)
    , "    x <.> one = " ++ show l
    , "    one <.> x = " ++ show r ]

-- | Annihilation of '<.>' by 'zero'.
annihilate :: (Eq a, Semiring a, Show a) => a -> Either String String
annihilate (x :: a) = if res then Right s else Left s where
  res = l == zero && r == zero
  l = x <.> zero
  r = zero <.> x
  s = unlines
    [ "zero does " ++ (if res then "" else "not ") ++ "annihilate with <.>."
    , "    Law:"
    , "        x <.> zero = zero <.> x = zero"
    , "    x = " ++ show x
    , "    zero = " ++ show (zero :: a)
    , "    x <.> zero = " ++ show l
    , "    zero <.> x = " ++ show r ]

unaryLaws :: (Eq a, Semiring a, Show a) => a -> Either String String
unaryLaws x = fmap unlines (sequence [plusId x, mulId x, annihilate x])

binaryLaws :: (Eq a, Semiring a, Show a)
           => a -> a -> Either String String
binaryLaws = plusComm

ternaryLaws :: (Eq a, Semiring a, Show a)
            => a -> a -> a -> Either String String
ternaryLaws x y z =
  fmap unlines (sequence [ plusAssoc x y z
                         , mulAssoc x y z
                         , mulDistribL x y z
                         , mulDistribR x y z])
