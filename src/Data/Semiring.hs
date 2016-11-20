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
  , add
  , mul
  , Max(..)
  , Min(..)
  ) where


import           Data.Functor.Const    (Const (..))
import           Data.Functor.Identity (Identity (..))

import           Data.Complex          (Complex)
import           Data.Fixed            (Fixed, HasResolution)
import           Data.Ratio            (Ratio)
import           Numeric.Natural       (Natural)

import           Data.Set              (Set)
import qualified Data.Set              as Set

import           Data.Int              (Int16, Int32, Int64, Int8)
import           Data.Word             (Word16, Word32, Word64, Word8)
import           Foreign.C.Types       (CChar, CClock, CDouble, CFloat, CInt,
                                        CIntMax, CIntPtr, CLLong, CLong,
                                        CPtrdiff, CSChar, CSUSeconds, CShort,
                                        CSigAtomic, CSize, CTime, CUChar, CUInt,
                                        CUIntMax, CUIntPtr, CULLong, CULong,
                                        CUSeconds, CUShort, CWchar)

import           Foreign.Ptr           (IntPtr, WordPtr)
import           System.Posix.Types    (CCc, CDev, CGid, CIno, CMode, CNlink,
                                        COff, CPid, CRLim, CSpeed, CSsize,
                                        CTcflag, CUid, Fd)

import           Data.Monoid

import           Control.Applicative   (liftA2)
import           Data.Coerce           (coerce)
import           GHC.Generics          (Generic, Generic1)


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
-- * @a '<.>' (b '<+>' c) = (a '<.>' b) '<+>' (a '<.>' c)@
-- * @(a '<+>' b) '<.>' c = (a '<.>' c) '<+>' (b '<.>' c)@
--
-- == Annihilation
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

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
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

cartProd :: (Ord a, Monoid a) => Set a -> Set a -> Set a
cartProd xs ys =
  Set.foldl' (\a x ->
                Set.foldl' (flip (Set.insert . mappend x)) a ys)
  Set.empty xs

-- | The 'Set' 'Semiring' is 'Data.Set.union' for '<+>', and a Cartesian
-- product for '<.>'.
instance (Ord a, Monoid a) => Semiring (Set a) where
  (<.>) = cartProd
  (<+>) = Set.union
  zero = Set.empty
  one = Set.singleton mempty

-- | A polynomial in /x/ can be defined as a list of its coefficients,
-- where the /i/th element is the coefficient of /x^i/. This is the
-- semiring for such a list. Adapted from <https://pdfs.semanticscholar.org/702d/348c32133997e992db362a19697d5607ab32.pdf here>.
instance Semiring a => Semiring [a] where
  one = [one]
  zero = []
  [] <+> ys = ys
  xs <+> [] = xs
  (x:xs) <+> (y:ys) = (x <+> y) : (xs <+> ys)
  [] <.> _ = []
  _ <.> [] = []
  (x:xs) <.> (y:ys) =
    (x <.> y) : (map (x <.>) ys <+> map (<.> y) xs <+> (xs <.> ys))

------------------------------------------------------------------------
-- Addition and multiplication newtypes
------------------------------------------------------------------------

type WrapBinary f a = (a -> a -> a) -> f a -> f a -> f a

-- | Monoid under '<+>'. Analogous to 'Data.Monoid.Sum', but uses the
-- 'Semiring' constraint, rather than 'Num'.
newtype Add a = Add
  { getAdd :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum)

-- | Monoid under '<.>'. Analogous to 'Data.Monoid.Product', but uses the
-- 'Semiring' constraint, rather than 'Num'.
newtype Mul a = Mul
  { getMul :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum)

instance Functor Add where fmap = coerce

instance Functor Mul where fmap = coerce

instance Foldable Add where
  foldr =
    (coerce :: ((a -> b -> c) -> (b ->     a -> c))
            ->  (a -> b -> c) -> (b -> Add a -> c)
    ) flip
  foldl = coerce
  foldMap = coerce
  length = const 1

instance Foldable Mul where
  foldr =
    (coerce :: ((a -> b -> c) -> (b ->     a -> c))
            ->  (a -> b -> c) -> (b -> Mul a -> c)
    ) flip
  foldl = coerce
  foldMap = coerce
  length = const 1

instance Applicative Add where
  pure = coerce
  (<*>) =
    (coerce :: (    (a -> b) ->     a ->     b)
            -> (Add (a -> b) -> Add a -> Add b)
    ) ($)

instance Applicative Mul where
  pure = coerce
  (<*>) =
    (coerce :: (    (a -> b) ->     a ->     b)
            -> (Mul (a -> b) -> Mul a -> Mul b)
    ) ($)

instance Monad Add where (>>=) = flip coerce

instance Monad Mul where (>>=) = flip coerce

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

------------------------------------------------------------------------
-- Addition and multiplication folds
------------------------------------------------------------------------

-- | Takes the sum of the elements of a 'Foldable'. Analogous to 'sum'
-- on numbers, or 'or' on 'Bool's.
add :: (Foldable f, Semiring a) => f a -> a
add = getAdd . foldMap Add

-- | Takes the product of the elements of a 'Foldable'. Analogous to
-- 'product' on numbers, or 'and' on 'Bool's.
mul :: (Foldable f, Semiring a) => f a -> a
mul = getMul . foldMap Mul

------------------------------------------------------------------------
-- Ord wrappers
------------------------------------------------------------------------


-- | The "<https://ncatlab.org/nlab/show/tropical+semiring Tropical>" or
-- min-plus semiring. It is a semiring where:
--
-- @'<+>'  = 'min'
--'zero' = -∞ -- represented by 'Nothing'
--'<.>'  = '<+>'
--'one'  = 'zero'@
--
-- Note that we can't use 'Data.Semigroup.Min' from 'Data.Semigroup'
-- because annihilation needs to hold:
--
-- @-∞ '<+>' x = x '<+>' -∞ = -∞@
--
-- Taking -∞ to be 'minBound' would break the above law. Using 'Nothing'
-- to represent it follows the law.
newtype Min a = Min
  { getMin :: Maybe a
  } deriving (Eq, Ord, Read, Show, Generic, Generic1, Functor
             ,Foldable)

-- | The "<https://ncatlab.org/nlab/show/max-plus+algebra Arctic>"
-- or max-plus semiring. It is a semiring where:
--
-- @'<+>'  = 'max'
--'zero' = ∞ -- represented by 'Nothing'
--'<.>'  = '<+>'
--'one'  = 'zero'@
--
-- Note that we can't use 'Data.Semigroup.Max' from 'Data.Semigroup'
-- because annihilation needs to hold:
--
-- @∞ '<+>' x = x '<+>' ∞ = ∞@
--
-- Taking ∞ to be 'maxBound' would break the above law. Using 'Nothing'
-- to represent it follows the law.
newtype Max a = Max
  { getMax :: Maybe a
  } deriving (Eq, Ord, Read, Show, Generic, Generic1, Functor
             ,Foldable)

instance Applicative Max where
  pure = (coerce :: (a -> Maybe a) -> (a -> Max a)) Just
  (<*>) = (coerce :: (Maybe (a -> b) -> Maybe a -> Maybe b)
                  ->  Max   (a -> b) -> Max   a -> Max   b
          ) (<*>)

instance Applicative Min where
  pure = (coerce :: (a -> Maybe a) -> (a -> Min a)) Just
  (<*>) = (coerce :: (Maybe (a -> b) -> Maybe a -> Maybe b)
                  ->  Min   (a -> b) -> Min   a -> Min   b
          ) (<*>)

instance Monad Max where
  (>>=) = (coerce :: (Maybe a -> (a -> Maybe b) -> Maybe b)
                  ->  Max   a -> (a -> Max   b) -> Max   b
          ) (>>=)

instance Monad Min where
  (>>=) = (coerce :: (Maybe a -> (a -> Maybe b) -> Maybe b)
                  ->  Min   a -> (a -> Min   b) -> Min   b
          ) (>>=)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  Max Nothing `mappend` x = x
  x `mappend` Max Nothing = x
  Max (Just x) `mappend` Max (Just y) = (Max . Just) (min x y)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  Min Nothing `mappend` x = x
  x `mappend` Min Nothing = x
  Min (Just x) `mappend` Min (Just y) = (Min . Just) (min x y)

instance (Semiring a, Ord a) => Semiring (Max a) where
  (<+>) = mappend
  zero = mempty
  (<.>) = liftA2 (<+>)
  one = Max (Just zero)

instance (Semiring a, Ord a) => Semiring (Min a) where
  (<+>) = mappend
  zero = mempty
  (<.>) = liftA2 (<+>)
  one = Min (Just zero)

------------------------------------------------------------------------
-- (->) instance
------------------------------------------------------------------------

-- | The @(->)@ instance is analogous to the one for 'Monoid'.
instance Semiring b => Semiring (a -> b) where
  zero = const zero
  one = const one
  (f <+> g) x = f x <+> g x
  (f <.> g) x = f x <.> g x

------------------------------------------------------------------------
-- Instances for Bool wrappers
------------------------------------------------------------------------

instance Semiring Any where
  (<+>) = coerce (||)
  zero = Any False
  (<.>) = coerce (&&)
  one = Any True

instance Semiring All where
  (<+>) = coerce (||)
  zero = All False
  (<.>) = coerce (&&)
  one = All True

------------------------------------------------------------------------
-- Boring instances
------------------------------------------------------------------------

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
instance Semiring Float
instance Semiring Double
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

------------------------------------------------------------------------
-- Very boring instances
------------------------------------------------------------------------
instance (Semiring a, Semiring b) => Semiring (a,b) where
        zero = (zero, zero)
        (a1,b1) <+> (a2,b2) =
                (a1 <+> a2, b1 <+> b2)
        one = (one, one)
        (a1,b1) <.> (a2,b2) =
                (a1 <.> a2, b1 <.> b2)

instance (Semiring a, Semiring b, Semiring c) => Semiring (a,b,c) where
        zero = (zero, zero, zero)
        (a1,b1,c1) <+> (a2,b2,c2) =
                (a1 <+> a2, b1 <+> b2, c1 <+> c2)
        one = (one, one, one)
        (a1,b1,c1) <.> (a2,b2,c2) =
                (a1 <.> a2, b1 <.> b2, c1 <.> c2)

instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a,b,c,d) where
        zero = (zero, zero, zero, zero)
        (a1,b1,c1,d1) <+> (a2,b2,c2,d2) =
                (a1 <+> a2, b1 <+> b2,
                 c1 <+> c2, d1 <+> d2)
        one = (one, one, one, one)
        (a1,b1,c1,d1) <.> (a2,b2,c2,d2) =
                (a1 <.> a2, b1 <.> b2, c1 <.> c2, d1 <.> d2)

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e) =>
                Semiring (a,b,c,d,e) where
        zero = (zero, zero, zero, zero, zero)
        (a1,b1,c1,d1,e1) <+> (a2,b2,c2,d2,e2) =
                (a1 <+> a2, b1 <+> b2, c1 <+> c2,
                 d1 <+> d2, e1 <+> e2)
        one = (one, one, one, one, one)
        (a1,b1,c1,d1,e1) <.> (a2,b2,c2,d2,e2) =
                (a1 <.> a2, b1 <.> b2, c1 <.> c2,
                 d1 <.> d2, e1 <.> e2)
