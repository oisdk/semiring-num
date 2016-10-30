{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

{-|
Module: Data.Semiring
Description: Haskell semirings
License: MIT
Maintainer: mail@doisinkidney.com
Stability: experimental
-}

module Data.Semiring
  ( Semiring(..)
  , Add(..)
  , Mul(..)
  , Bottleneck(..)
  , Division(..)
  ) where

import           Data.Coerce
import           Data.Complex
import           Data.Fixed
import           Data.Functor.Const
import           Data.Functor.Identity
import           Data.Int
import           Data.Monoid
import           Data.Ratio
import           Data.Semigroup
import           Data.Set              (Set)
import qualified Data.Set              as Set
import           Data.Word
import           Foreign.C.Types
import           Foreign.Ptr
import           GHC.Generics
import           Numeric.Natural
import           System.Posix.Types

-- | A <https://en.wikipedia.org/wiki/Semiring Semiring> is like the
-- the combination of two 'Data.Monoid.Monoid's. The first
-- is called '<+>'; it has the identity element 'zero', and it is
-- commutative. The second is called '<.>'; it has identity element 'one',
-- and it must distribute over '<+>'.
--
-- = Laws
-- == Normal 'Precursor.Algebra.Monoid.Monoid' laws
-- * @(a '<+>' b) '<+>' c = a '<+>' (b '<+>' c)@
-- * @'zero' '<+>' a = a '<+>' 'zero' = a@
-- * @(a '<.>' b) '<.>' c = a '<.>' (b '<.>' c)@
-- * @'one' '<.>' a = a '<.>' 'one' = a@
--
-- == Commutativity of '<+>'
-- * @a '<+>' b = b '<+>' a@
--
-- == Distribution of '<.>' over '<+>'
-- * @a'<.>'(b '<+>' c) = (a'<.>'b) '<+>' (a'<.>'c)@
-- * @(a '<+>' b)'<.>'c = (a'<.>'c) '<+>' (b'<.>'c)@
--
-- Another useful law, annihilation, may be deduced from the axioms
-- above:
--
-- * @'zero' '<.>' a = a '<.>' 'zero' = 'zero'@
class Semiring a where
  -- | The identity of '<+>'.
  zero :: a
  -- | The identity of '<.>'.
  one :: a
  -- | An associative binary operation, which distributes over '<+>'.
  infixl 7 <.>
  (<.>) :: a -> a -> a
  -- | An associative, commutative binary operation.
  infixl 6 <+>
  (<+>) :: a -> a -> a

  default zero :: Num a => a
  default one :: Num a => a
  default (<+>) :: Num a => a -> a -> a
  default (<.>) :: Num a => a -> a -> a

  zero = 0
  one = 1
  (<+>) = (+)
  (<.>) = (*)

instance Semiring Bool where
  one = True
  zero = False
  (<+>) = (||)
  (<.>) = (&&)

instance Semiring () where
  one = ()
  zero = ()
  _ <+> _ = ()
  _ <.> _ = ()

type WrapBinary f a = (a -> a -> a) -> f a -> f a -> f a

newtype Add a = Add
  { getAdd :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

newtype Mul a = Mul
  { getMul :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

instance Functor Add where fmap = coerce

instance Functor Mul where fmap = coerce

instance Foldable Add where
  foldr   =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Add a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1

instance Foldable Mul where
  foldr   =
    (coerce :: ((a -> b -> c) -> (b -> a -> c))
            -> (a -> b -> c)
            -> (b -> Mul a -> c)) flip
  foldl   = coerce
  foldMap = coerce
  length  = const 1

instance Applicative Add where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Add (a -> b) -> Add a -> Add b)) ($)

instance Applicative Mul where
  pure = coerce
  (<*>) =
    (coerce :: ((a -> b) -> a -> b)
            -> (Mul (a -> b) -> Mul a -> Mul b)) ($)

instance Monad Add where
  (>>=) = flip coerce

instance Monad Mul where
  (>>=) = flip coerce

instance Semiring a => Monoid (Add a) where
  mempty = Add zero
  mappend = (coerce :: WrapBinary Add a) (<+>)

instance Semiring a => Monoid (Mul a) where
  mempty = Mul one
  mappend = (coerce :: WrapBinary Mul a) (<.>)

instance Semiring a => Semiring (Add a) where
  zero = Add zero
  one = Add one
  (<+>) = (coerce :: WrapBinary Add a) (<+>)
  (<.>) = (coerce :: WrapBinary Add a) (<.>)

instance Semiring a => Semiring (Mul a) where
  zero = Mul zero
  one = Mul one
  (<+>) = (coerce :: WrapBinary Mul a) (<+>)
  (<.>) = (coerce :: WrapBinary Mul a) (<.>)

instance (Ord a, Bounded a, Semiring a) => Semiring (Max a) where
  (<+>) = mappend
  zero = mempty
  (<.>) = (coerce :: WrapBinary Max a) (<+>)
  one = Max zero

instance (Ord a, Bounded a, Semiring a) => Semiring (Min a) where
  (<+>) = mappend
  zero = mempty
  (<.>) = (coerce :: WrapBinary Min a) (<+>)
  one = Min zero

instance Semiring b => Semiring (a -> b) where
  zero = const zero
  one  = const one
  (f <+> g) x = f x <+> g x
  (f <.> g) x = f x <.> g x

-- | Needs a to be a commutative Monoid
instance Monoid a => Semiring (Endo a) where
  (<.>) = mappend
  one = mempty
  Endo f <+> Endo g = Endo (\x -> f x `mappend` g x)
  zero = Endo (const mempty)

newtype Bottleneck a = Bottleneck
  { getBottleneck :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

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
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num)

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

cartProd :: (Ord a, Monoid a) => Set a -> Set a -> Set a
cartProd xs ys = Set.foldl' (\a x -> Set.foldl' (flip (Set.insert . mappend x)) a ys) Set.empty xs

instance (Ord a, Monoid a) => Semiring (Set a) where
  (<.>) = cartProd
  (<+>) = Set.union
  zero = Set.empty
  one = Set.singleton mempty

instance Semiring Int
instance Semiring Int8
instance Semiring Int16
instance Semiring Int32
instance Semiring Int64
instance Semiring Integer
instance Semiring Word
instance Semiring Word8
instance Semiring Word16
instance Semiring Word32
instance Semiring Word64
instance Semiring CUIntMax
instance Semiring CIntMax
instance Semiring CUIntPtr
instance Semiring CIntPtr
instance Semiring CSUSeconds
instance Semiring CUSeconds
instance Semiring CTime
instance Semiring CClock
instance Semiring CSigAtomic
instance Semiring CWchar
instance Semiring CSize
instance Semiring CPtrdiff
instance Semiring CDouble
instance Semiring CFloat
instance Semiring CULLong
instance Semiring CLLong
instance Semiring CULong
instance Semiring CLong
instance Semiring CUInt
instance Semiring CInt
instance Semiring CUShort
instance Semiring CShort
instance Semiring CUChar
instance Semiring CSChar
instance Semiring CChar
instance Semiring IntPtr
instance Semiring WordPtr
instance Semiring Fd
instance Semiring CRLim
instance Semiring CTcflag
instance Semiring CSpeed
instance Semiring CCc
instance Semiring CUid
instance Semiring CNlink
instance Semiring CGid
instance Semiring CSsize
instance Semiring CPid
instance Semiring COff
instance Semiring CMode
instance Semiring CIno
instance Semiring CDev
instance Semiring Natural
instance Integral a => Semiring (Ratio a)
deriving instance Semiring a => Semiring (Product a)
deriving instance Semiring a => Semiring (Sum a)
instance RealFloat a => Semiring (Complex a)
instance HasResolution a => Semiring (Fixed a)
deriving instance Semiring a => Semiring (Identity a)
deriving instance Semiring a => Semiring (Const a b)
