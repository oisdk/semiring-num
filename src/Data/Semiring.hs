{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
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
  , StarSemiring(..)
  , HasPositiveInfinity(..)
  , HasNegativeInfinity(..)
  , PositiveInfinite(..)
  , NegativeInfinite(..)
  , Infinite(..)
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

import           Data.Semigroup        hiding (Max (..), Min (..))

import           Control.Applicative   (liftA2)
import           Data.Coerce           (coerce)
import           GHC.Generics          (Generic, Generic1)

import           Data.Typeable         (Typeable)
import           Foreign.Storable      (Storable)

import           Data.Semiring.Infinite
import           Data.Semiring.TH


-- | A <https://en.wikipedia.org/wiki/Semiring Semiring> is like the
-- the combination of two 'Data.Monoid.Monoid's. The first
-- is called '<+>'; it has the identity element 'zero', and it is
-- commutative. The second is called '<.>'; it has identity element 'one',
-- and it must distribute over '<+>'.
--
-- = Laws
-- == Normal 'Monoid' laws
--
-- @(a '<+>' b) '<+>' c = a '<+>' (b '<+>' c)
--'zero' '<+>' a = a '<+>' 'zero' = a
--(a '<.>' b) '<.>' c = a '<.>' (b '<.>' c)
--'one' '<.>' a = a '<.>' 'one' = a@
--
-- == Commutativity of '<+>'
-- @a '<+>' b = b '<+>' a@
--
-- == Distribution of '<.>' over '<+>'
-- @a '<.>' (b '<+>' c) = (a '<.>' b) '<+>' (a '<.>' c)
--(a '<+>' b) '<.>' c = (a '<.>' c) '<+>' (b '<.>' c)@
--
-- == Annihilation
-- @'zero' '<.>' a = a '<.>' 'zero' = 'zero'@
--
-- An ordered semiring follows the laws:
-- @x '<=' y => x '<+>' z '<=' y '<+>' z@
-- @x '<=' y => x '<+>' z '<=' y '<+>' z@
-- @'zero' '<=' z '&&' x '<=' y => x '<.>' z '<=' y '<.>' z '&&' z '<.>' x '<=' z '<.>' y@
class Semiring a  where
    -- | The identity of '<+>'.
    zero
        :: a
    -- | The identity of '<.>'.
    one
        :: a
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
    {-# INLINE zero #-}
    one = 1
    {-# INLINE one #-}
    (<+>) = (+)
    {-# INLINE (<+>) #-}
    (<.>) = (*)
    {-# INLINE (<.>) #-}

-- | A <https://en.wikipedia.org/wiki/Semiring#Star_semirings Star semiring>
-- adds one operation, 'star' to a 'Semiring', such that it follows the
-- law:
--
-- @'star' x = 'one' '<+>' x '<.>' 'star' x = 'one' '<+>' 'star' x '<.>' x@
--
-- For the semiring of types, this is equivalent to a list. When looking
-- at the 'Applicative' and 'Control.Applicative.Alternative' classes as
-- (near-) semirings, this is equivalent to the
-- 'Control.Applicative.many' operation.
--
-- Another operation, 'plus', can be defined in relation to 'star':
--
-- @'plus' x = x '<.>' 'star' x@
--
-- This should be recognizable as a non-empty list on types, or the
-- 'Control.Applicative.some' operation in
-- 'Control.Applicative.Alternative'.
class Semiring a =>
      StarSemiring a  where
    {-# MINIMAL star | plus #-}
    star :: a -> a
    plus :: a -> a
    star x = one <+> plus x
    plus x = x <.> star x

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
type CoerceBinary a b = (a -> a -> a) -> b -> b -> b

instance Semiring Bool where
    one = True
    zero = False
    (<+>) = (||)
    (<.>) = (&&)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance StarSemiring Bool where
    star _ = True
    plus = id
    {-# INLINE star #-}
    {-# INLINE plus #-}

-- | Not lawful. Only for convenience.
instance Semiring a =>
         Semiring (NegativeInfinite a) where
    one = pure one
    zero = pure zero
    (<+>) =
        (coerce :: CoerceBinary (NegativeInfinite (Add a)) (NegativeInfinite a))
            mappend
    (<.>) = liftA2 (<.>)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

-- | Not lawful. Only for convenience.
instance Semiring a =>
         Semiring (PositiveInfinite a) where
    one = pure one
    zero = pure zero
    (<+>) =
        (coerce :: CoerceBinary (PositiveInfinite (Add a)) (PositiveInfinite a))
            mappend
    (<.>) = liftA2 (<.>)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

-- | Not lawful. Only for convenience.
instance Semiring a =>
         Semiring (Infinite a) where
    one = pure one
    zero = pure zero
    (<+>) = (coerce :: CoerceBinary (Infinite (Add a)) (Infinite a)) mappend
    (<.>) = liftA2 (<.>)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance (Eq a, Semiring a) =>
         StarSemiring (PositiveInfinite a) where
    star (PosFinite x)
      | x == zero = one
    star _ = PositiveInfinity

instance Semiring () where
    one = ()
    zero = ()
    _ <+> _ = ()
    _ <.> _ = ()
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance StarSemiring () where
    star _ = ()
    plus _ = ()
    {-# INLINE star #-}
    {-# INLINE plus #-}

-- | A polynomial in /x/ can be defined as a list of its coefficients,
-- where the /i/th element is the coefficient of /x^i/. This is the
-- semiring for such a list. Adapted from <https://pdfs.semanticscholar.org/702d/348c32133997e992db362a19697d5607ab32.pdf here>.
instance Semiring a =>
         Semiring [a] where
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
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,Semiring,StarSemiring)

-- | Monoid under '<.>'. Analogous to 'Data.Monoid.Product', but uses the
-- 'Semiring' constraint, rather than 'Num'.
newtype Mul a = Mul
    { getMul :: a
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,Semiring,StarSemiring)

instance Semiring a =>
         Semigroup (Add a) where
    (<>) = (coerce :: WrapBinary Add a) (<+>)
    {-# INLINE (<>) #-}

instance Semiring a =>
         Semigroup (Mul a) where
    (<>) = (coerce :: WrapBinary Mul a) (<.>)
    {-# INLINE (<>) #-}

instance Semiring a =>
         Monoid (Add a) where
    mempty = Add zero
    mappend = (<>)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance Semiring a =>
         Monoid (Mul a) where
    mempty = Mul one
    mappend = (<>)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

------------------------------------------------------------------------
-- Addition and multiplication folds
------------------------------------------------------------------------
-- | Takes the sum of the elements of a 'Foldable'. Analogous to 'sum'
-- on numbers, or 'or' on 'Bool's.
--
-- >>> add [1..5]
-- 15
-- >>> add [False, False]
-- False
-- >>> add [False, True]
-- True
-- >>> add [True, undefined]
-- True
add
    :: (Foldable f, Semiring a)
    => f a -> a
add = getAdd . foldMap Add

-- | Takes the product of the elements of a 'Foldable'. Analogous to
-- 'product' on numbers, or 'and' on 'Bool's.
--
-- >>> mul [1..5]
-- 120
-- >>> mul [True, True]
-- True
-- >>> mul [True, False]
-- False
-- >>> mul [False, undefined]
-- False
mul
    :: (Foldable f, Semiring a)
    => f a -> a
mul = getMul . foldMap Mul

------------------------------------------------------------------------
-- Ord wrappers
------------------------------------------------------------------------
-- | The "<https://ncatlab.org/nlab/show/tropical+semiring Tropical>" or
-- min-plus semiring. It is a semiring where:
--
-- @'<+>'  = 'min'
--'zero' = ∞ -- represented by 'Nothing'
--'<.>'  = '<+>'
--'one'  = 'zero'@
--
-- Note that we can't use 'Data.Semigroup.Min' from 'Data.Semigroup'
-- because annihilation needs to hold:
--
-- @∞ '<+>' x = x '<+>' ∞ = ∞@
--
-- Taking ∞ to be 'maxBound' would break the above law. Using 'Nothing'
-- to represent it follows the law.
newtype Min a = Min
    { getMin :: a
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable)

-- | The "<https://ncatlab.org/nlab/show/max-plus+algebra Arctic>"
-- or max-plus semiring. It is a semiring where:
--
-- @'<+>'  = 'max'
--'zero' = -∞ -- represented by 'Nothing'
--'<.>'  = '<+>'
--'one'  = 'zero'@
--
-- Note that we can't use 'Data.Semigroup.Max' from 'Data.Semigroup'
-- because annihilation needs to hold:
--
-- @-∞ '<+>' x = x '<+>' -∞ = -∞@
--
-- Taking -∞ to be 'minBound' would break the above law. Using 'Nothing'
-- to represent it follows the law.
newtype Max a = Max
    { getMax :: a
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable)

instance Ord a =>
         Semigroup (Max a) where
    (<>) = (coerce :: WrapBinary Max a) max
    {-# INLINE (<>) #-}

instance Ord a =>
         Semigroup (Min a) where
    (<>) = (coerce :: WrapBinary Min a) min
    {-# INLINE (<>) #-}

-- | >>> (getMax . foldMap Max) [1..10]
-- 10.0
instance (Ord a, HasNegativeInfinity a) =>
         Monoid (Max a) where
    mempty = Max negativeInfinity
    mappend = (<>)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

-- | >>> (getMin . foldMap Min) [1..10]
-- 1.0
instance (Ord a, HasPositiveInfinity a) =>
         Monoid (Min a) where
    mempty = Min positiveInfinity
    mappend = (<>)
    {-# INLINE mempty #-}
    {-# INLINE mappend #-}

instance (Semiring a, Ord a, HasNegativeInfinity a) =>
         Semiring (Max a) where
    (<+>) = mappend
    zero = mempty
    (<.>) = (coerce :: WrapBinary Max a) (<+>)
    one = Max zero
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance (Semiring a, Ord a, HasPositiveInfinity a) =>
         Semiring (Min a) where
    (<+>) = mappend
    zero = mempty
    (<.>) = (coerce :: WrapBinary Min a) (<+>)
    one = Min zero
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance (Semiring a, Ord a, HasPositiveInfinity a, HasNegativeInfinity a) =>
         StarSemiring (Max a) where
    star (Max x)
      | x > zero = Max positiveInfinity
      | otherwise = Max zero

instance (Semiring a, Ord a, HasPositiveInfinity a, HasNegativeInfinity a) =>
         StarSemiring (Min a) where
    star (Min x)
      | x < zero = Min negativeInfinity
      | otherwise = Min zero

------------------------------------------------------------------------
-- (->) instance
------------------------------------------------------------------------
-- | The @(->)@ instance is analogous to the one for 'Monoid'.
instance Semiring b =>
         Semiring (a -> b) where
    zero = const zero
    one = const one
    (f <+> g) x = f x <+> g x
    (f <.> g) x = f x <.> g x

instance StarSemiring b =>
         StarSemiring (a -> b) where
    star f x = star (f x)
    plus f x = plus (f x)

------------------------------------------------------------------------
-- Endo instance
------------------------------------------------------------------------
-- | This is /not/ a true semiring. In particular, it requires the
-- underlying monoid to be commutative, and even then, it is only a near
-- semiring. It is, however, extremely useful. For instance, this type:
--
-- @forall a. 'Endo' ('Endo' a)@
--
-- Is a valid encoding of church numerals, with addition and
-- multiplication being their semiring variants.
instance Monoid a =>
         Semiring (Endo a) where
    zero = Endo mempty
    Endo f <+> Endo g = Endo (f `mappend` g)
    one = mempty
    (<.>) = mappend
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance (Monoid a, Eq a) =>
         StarSemiring (Endo a) where
    star (Endo f) = Endo converge
      where
        converge x = go x
          where
            go inp =
                mappend
                    x
                    (if inp == next
                         then inp
                         else go next)
              where
                next = mappend x (f inp)

------------------------------------------------------------------------
-- Instances for Bool wrappers
------------------------------------------------------------------------
instance Semiring Any where
    (<+>) = coerce (||)
    zero = Any False
    (<.>) = coerce (&&)
    one = Any True
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance StarSemiring Any where
    star _ = Any True
    plus = id
    {-# INLINE star #-}
    {-# INLINE plus #-}

instance Semiring All where
    (<+>) = coerce (||)
    zero = All False
    (<.>) = coerce (&&)
    one = All True
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance StarSemiring All where
    star _ = All True
    plus = id
    {-# INLINE star #-}
    {-# INLINE plus #-}

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

$(traverse semiringIns [2..9])
$(traverse starIns [2..9])
