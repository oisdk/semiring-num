{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
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
  , Log(..)
  ) where

import           Data.Coerce
import           Data.Semiring
import           GHC.Generics

import           Data.Typeable    (Typeable)
import           Foreign.Storable (Storable)

type WrapBinary f a = (a -> a -> a) -> f a -> f a -> f a

-- | Useful for some constraint problems.
--
-- @('<+>') = 'max'
--('<.>') = 'min'
--'zero'  = 'minBound'
--'one'   = 'maxBound'@
newtype Bottleneck a = Bottleneck
  { getBottleneck :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Typeable, Storable, Fractional, Real, RealFrac)

instance (Bounded a, Ord a) => Semiring (Bottleneck a) where
  (<+>) = (coerce :: WrapBinary Bottleneck a) max
  (<.>) = (coerce :: WrapBinary Bottleneck a) min
  zero = Bottleneck minBound
  one  = Bottleneck maxBound

-- | Positive numbers only.
--
-- @('<+>') = 'gcd'
--('<.>') = 'lcm'
--'zero'  = 'zero'
--'one'   = 'one'@
newtype Division a = Division
  { getDivision :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Typeable, Storable, Fractional, Real, RealFrac)

-- | Only expects positive numbers
instance (Integral a, Semiring a) => Semiring (Division a) where
  (<+>) = (coerce :: WrapBinary Division a) gcd
  (<.>) = (coerce :: WrapBinary Division a) lcm
  zero = Division zero
  one = Division one

-- | <https://en.wikipedia.org/wiki/Semiring#cite_ref-droste_14-0 Wikipedia>
-- has some information on this. Also
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.304.6152&rep=rep1&type=pdf this>
-- paper.
--
-- @('<+>')   = 'max'
--x '<.>' y = 'max' 0 (x '+' y '-' 1)
--'zero'    = 'zero'
--'one'     = 'one'@
newtype Łukasiewicz a = Łukasiewicz
  { getŁukasiewicz :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Typeable, Storable, Fractional, Real, RealFrac)

instance (Ord a, Num a) => Semiring (Łukasiewicz a) where
  (<+>) = (coerce :: WrapBinary Łukasiewicz a) max
  (<.>) = (coerce :: WrapBinary Łukasiewicz a) (\x y -> max 0 (x + y - 1))
  zero = Łukasiewicz 0
  one  = Łukasiewicz 1

-- | <https://en.wikipedia.org/wiki/Semiring#cite_ref-droste_14-0 Wikipedia>
-- has some information on this. Also
-- <http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.304.6152&rep=rep1&type=pdf this>
-- paper. Apparently used for probabilistic parsing.
--
-- @('<+>') = 'max'
--('<.>') = ('<.>')
--'zero'  = 'zero'
--'one'   = 'one'@
newtype Viterbi a = Viterbi
  { getViterbi :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Typeable, Storable, Fractional, Real, RealFrac)

instance (Ord a, Semiring a) => Semiring (Viterbi a) where
  (<+>) = (coerce :: WrapBinary Viterbi a) max
  (<.>) = (coerce :: WrapBinary Viterbi a) (<.>)
  zero = Viterbi zero
  one  = Viterbi one

-- | Useful for optimizing multiplication, or working with large numbers.
--
-- @('<.>')   = ('+')
--x '<+>' y = -('log' ('exp' (-x) + 'exp' (-y)))
--'zero'    = ∞ -- represented by 'Nothing'
--'one'     = 0@
newtype Log a = Log
  { getLog :: Maybe a
  } deriving (Eq, Read, Show, Generic, Generic1, Typeable, Functor
             ,Foldable, Applicative, Monad)

instance (Semiring a, Floating a) => Semiring (Log a) where
  zero = Log Nothing
  one = Log (Just zero)
  Log (Just x) <.> Log (Just y) = Log (Just (x + y))
  _ <.> _ = Log Nothing
  Log Nothing <+> y = y
  x <+> Log Nothing = x
  Log (Just x) <+> Log (Just y)
    = Log (Just (-(log (exp (-x) + exp (-y)))))

instance Ord a => Ord (Log a) where
  compare (Log Nothing) (Log Nothing)   = EQ
  compare (Log Nothing) _               = LT
  compare _ (Log Nothing)               = GT
  compare (Log (Just x)) (Log (Just y)) = compare x y

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
