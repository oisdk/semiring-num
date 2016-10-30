{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module: Data.Semiring
Description: Some interesting numeric semirings
License: MIT
Maintainer: mail@doisinkidney.com
Stability: experimental
-}
module Data.Semiring.Numeric
  ( Bottleneck(..)
  , Division(..)
  , Łukasiewicz(..)
  , Viterbi(..)
  ) where

import           Data.Coerce
import           Data.Semiring
import           GHC.Generics
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen

type WrapBinary f a = (a -> a -> a) -> f a -> f a -> f a

newtype Bottleneck a = Bottleneck
  { getBottleneck :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Arbitrary)

instance (Bounded a, Ord a) => Semiring (Bottleneck a) where
  (<+>) = (coerce :: WrapBinary Bottleneck a) max
  (<.>) = (coerce :: WrapBinary Bottleneck a) min
  zero = Bottleneck minBound
  one  = Bottleneck maxBound

instance Functor Bottleneck where fmap = coerce

instance Foldable Bottleneck where
  foldr   =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Bottleneck a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1

instance Applicative Bottleneck where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Bottleneck (a -> b) -> Bottleneck a -> Bottleneck b)) ($)

instance Monad Bottleneck where
  (>>=) = flip coerce

newtype Division a = Division
  { getDivision :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Arbitrary)

instance (Integral a, Semiring a) => Semiring (Division a) where
  (<+>) = (coerce :: WrapBinary Division a) lcm
  (<.>) = (coerce :: WrapBinary Division a) gcd
  zero = Division zero
  one  = Division one

instance Functor Division where fmap = coerce

instance Foldable Division where
  foldr   =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Division a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1

instance Applicative Division where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Division (a -> b) -> Division a -> Division b)) ($)

instance Monad Division where
  (>>=) = flip coerce

newtype Łukasiewicz a = Łukasiewicz
  { getŁukasiewicz :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

instance (Num a, Random a) => Arbitrary (Łukasiewicz a) where
  arbitrary = fmap Łukasiewicz (choose (0,1))

instance (Ord a, Num a) => Semiring (Łukasiewicz a) where
  (<+>) = (coerce :: WrapBinary Łukasiewicz a) max
  (<.>) = (coerce :: WrapBinary Łukasiewicz a) (\x y -> max 0 (x + y - 1))
  zero = Łukasiewicz 0
  one  = Łukasiewicz 1

instance Functor Łukasiewicz where fmap = coerce

instance Foldable Łukasiewicz where
  foldr =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Łukasiewicz a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1

instance Applicative Łukasiewicz where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Łukasiewicz (a -> b) -> Łukasiewicz a -> Łukasiewicz b)) ($)

instance Monad Łukasiewicz where
  (>>=) = flip coerce

newtype Viterbi a = Viterbi
  { getViterbi :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

instance (Semiring a, Random a) => Arbitrary (Viterbi a) where
  arbitrary = fmap Viterbi (choose (zero,one))

instance (Ord a, Semiring a) => Semiring (Viterbi a) where
  (<+>) = (coerce :: WrapBinary Viterbi a) max
  (<.>) = (coerce :: WrapBinary Viterbi a) (<.>)
  zero = Viterbi zero
  one  = Viterbi one

instance Functor Viterbi where fmap = coerce

instance Foldable Viterbi where
  foldr =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Viterbi a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1

instance Applicative Viterbi where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Viterbi (a -> b) -> Viterbi a -> Viterbi b)) ($)

instance Monad Viterbi where
  (>>=) = flip coerce
