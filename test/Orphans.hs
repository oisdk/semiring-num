{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Orphans where

import           Test.QuickCheck hiding (Positive(..), generate)
import           Test.SmallCheck.Series hiding (Positive(..))
import qualified Test.SmallCheck.Series as SC

import           Data.Semiring
import           Data.Semiring.Infinite
import           Data.Semiring.Free
import           Data.Semiring.Numeric
import qualified Data.Vector as Vector
import           Numeric.Natural
import           Numeric.Sized.WordOfSize
import           Data.Monoid
import           Numeric.Log

import           CompUtils

import           Data.Bool
import           GHC.TypeLits


instance Arbitrary a => Arbitrary (Add a) where
    arbitrary = Add <#$> arbitrary
    shrink = map Add #. shrink .# getAdd

instance CoArbitrary a => CoArbitrary (Add a) where
    coarbitrary = coarbitrary .# getAdd

instance Arbitrary a => Arbitrary (PositiveInfinite a) where
  arbitrary = fmap (maybe PositiveInfinity PosFinite) arbitrary

instance Arbitrary a => Arbitrary (NegativeInfinite a) where
  arbitrary = fmap (maybe NegativeInfinity NegFinite) arbitrary

instance Arbitrary a => Arbitrary (Infinite a) where
  arbitrary = fmap (either (bool Positive Negative) Finite) arbitrary

instance Arbitrary a => Arbitrary (Vector.Vector a) where
    arbitrary = fmap Vector.fromList arbitrary
    shrink = fmap Vector.fromList . shrink . Vector.toList

instance Testable (Either String String) where
  property = either (`counterexample` False) (const (property True))

instance Arbitrary (f (g a)) => Arbitrary (Matrix f g a) where
    arbitrary = fmap Matrix arbitrary
    shrink (Matrix xs) = fmap Matrix (shrink xs)

instance (Monad m, KnownNat n) => Serial m (WordOfSize n) where
  series = generate (`take` [minBound..maxBound])

instance KnownNat n => Arbitrary (WordOfSize n) where
  arbitrary = arbitraryBoundedEnum

instance KnownNat n => Semiring (WordOfSize n) where
  one = 1
  zero = 0
  (<+>) = (+)
  (<.>) = (*)

instance KnownNat n => DetectableZero (WordOfSize n) where
  isZero = (zero==)

instance (Monad m, Serial m a) => Serial m (PositiveInfinite a) where
  series = fmap (maybe PositiveInfinity PosFinite) series

instance (Monad m, Serial m a) => Serial m (NegativeInfinite a) where
  series = fmap (maybe NegativeInfinity NegFinite) series

instance (Monad m, Serial m a) => Serial m (Infinite a) where
  series = fmap (either (bool Positive Negative) Finite) series

instance Monad m => Serial m Natural where
  series = generate (`take` [0..])

instance Monad m => Serial m Any where
  series = fmap Any series

instance Monad m => Serial m All where
  series = fmap All series

instance (Monad m, Serial m a) => Serial m (Min a) where
  series = fmap Min series

instance (Monad m, Serial m a) => Serial m (Max a) where
  series = fmap Max series

instance (Ord a, Arbitrary a) => Arbitrary (Free a) where
  arbitrary = fmap Free arbitrary

instance (Serial m a, Monad m, Num a, Ord a) => Serial m (Division a) where
  series = fmap (Division . SC.getPositive) series

instance (Serial m a, Monad m) => Serial m (Łukasiewicz a) where
  series = fmap Łukasiewicz series

instance (Serial m a, Monad m) => Serial m (Viterbi a) where
  series = fmap Viterbi series

instance Serial m a => Serial m (Log a) where
    series = fmap Exp series
