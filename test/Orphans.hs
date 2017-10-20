{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}

module Orphans where

import           Test.QuickCheck hiding (Positive(..), generate)
import           Test.SmallCheck.Series hiding (Positive(..))
import qualified Test.SmallCheck.Series as SC

import           Data.Semiring
import           Data.Semiring.Infinite
import           Data.Semiring.Free
import           Data.Semiring.Numeric

import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Storable as Storable

import           Numeric.Natural
import           Numeric.Sized.WordOfSize
import           Data.Monoid
import           Numeric.Log

import           Data.Bool
import           GHC.TypeLits

deriving instance Arbitrary a => Arbitrary (Add a)
deriving instance CoArbitrary a => CoArbitrary (Add a)
deriving instance Arbitrary a => Arbitrary (Mul a)
deriving instance CoArbitrary a => CoArbitrary (Mul a)
deriving instance Arbitrary a => Arbitrary (Min a)
deriving instance CoArbitrary a => CoArbitrary (Min a)
deriving instance Arbitrary a   => Arbitrary (Max a)
deriving instance CoArbitrary a => CoArbitrary (Max a)
deriving instance Arbitrary a => Arbitrary (Bottleneck a)
deriving instance CoArbitrary a => CoArbitrary (Bottleneck a)
deriving instance Arbitrary a => Arbitrary (Division a)
deriving instance CoArbitrary a => CoArbitrary (Division a)
deriving instance Arbitrary a => Arbitrary (Łukasiewicz a)
deriving instance CoArbitrary a => CoArbitrary (Łukasiewicz a)
deriving instance Arbitrary a => Arbitrary (Viterbi a)
deriving instance CoArbitrary a => CoArbitrary (Viterbi a)
deriving instance Arbitrary a => Arbitrary (PosFrac a)
deriving instance CoArbitrary a => CoArbitrary (PosFrac a)
deriving instance Arbitrary a => Arbitrary (PosInt a)
deriving instance CoArbitrary a => CoArbitrary (PosInt a)

instance Arbitrary a => Arbitrary (PositiveInfinite a) where
  arbitrary = fmap (maybe PositiveInfinity PosFinite) arbitrary

instance Arbitrary a =>
         Arbitrary (NegativeInfinite a) where
    arbitrary = fmap (maybe NegativeInfinity NegFinite) arbitrary
    shrink =
        map (maybe NegativeInfinity NegFinite) .
        shrink . foldr (const . Just) Nothing

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

instance (Arbitrary a, Unboxed.Unbox a) =>
         Arbitrary (Unboxed.Vector a) where
    arbitrary = fmap Unboxed.fromList arbitrary
    shrink = map Unboxed.fromList . shrink . Unboxed.toList

instance (Arbitrary a, Storable.Storable a) =>
         Arbitrary (Storable.Vector a) where
    arbitrary = fmap Storable.fromList arbitrary
    shrink = map Storable.fromList . shrink . Storable.toList
