{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-|
Module: Data.Semiring.Numeric
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

-- | '<+>' is 'max', '<.>' is 'min'
newtype Bottleneck a = Bottleneck
  { getBottleneck :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Arbitrary)

instance (Bounded a, Ord a) => Semiring (Bottleneck a) where
  (<+>) = (coerce :: WrapBinary Bottleneck a) max
  (<.>) = (coerce :: WrapBinary Bottleneck a) min
  zero = Bottleneck minBound
  one  = Bottleneck maxBound

-- | '<+>' is 'gcd', '<.>' is 'lcm'. Positive numbers only.
newtype Division a = Division
  { getDivision :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

-- | Only expects positive numbers
instance (Integral a, Semiring a) => Semiring (Division a) where
  (<+>) = (coerce :: WrapBinary Division a) gcd
  (<.>) = (coerce :: WrapBinary Division a) lcm
  zero = Division zero
  one = Division one

instance (Integral a, Arbitrary a) => Arbitrary (Division a) where
  arbitrary = fmap (Division . abs) arbitrary

-- | <https://en.wikipedia.org/wiki/Semiring#cite_ref-droste_14-0 Wikipedia>
-- has some information on this. Also
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.304.6152&rep=rep1&type=pdf this>
-- paper.
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

-- | <https://en.wikipedia.org/wiki/Semiring#cite_ref-droste_14-0 Wikipedia>
-- has some information on this. Also
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.304.6152&rep=rep1&type=pdf this>
-- paper. Apparently used for probabilistic parsing.
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

------------------------------------------------------------------------
-- Boring instances
------------------------------------------------------------------------

instance Functor Bottleneck where fmap = coerce
instance Functor Division where fmap = coerce
instance Functor Łukasiewicz where fmap = coerce
instance Functor Viterbi where fmap = coerce

instance Foldable Bottleneck where
  foldr   =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Bottleneck a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1
  null _ = False

instance Foldable Division where
  foldr   =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Division a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1
  null _ = False

instance Foldable Łukasiewicz where
  foldr =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Łukasiewicz a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1
  null _ = False

instance Foldable Viterbi where
  foldr =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Viterbi a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1
  null _ = False

instance Applicative Bottleneck where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Bottleneck (a -> b) -> Bottleneck a -> Bottleneck b)) ($)

instance Applicative Łukasiewicz where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Łukasiewicz (a -> b) -> Łukasiewicz a -> Łukasiewicz b)) ($)

instance Applicative Division where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Division (a -> b) -> Division a -> Division b)) ($)

instance Applicative Viterbi where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Viterbi (a -> b) -> Viterbi a -> Viterbi b)) ($)

instance Monad Bottleneck where (>>=) = flip coerce
instance Monad Division where (>>=) = flip coerce
instance Monad Łukasiewicz where (>>=) = flip coerce
instance Monad Viterbi where (>>=) = flip coerce
