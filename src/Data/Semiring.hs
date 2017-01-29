{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE TemplateHaskell #-}
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

import           Data.Semigroup        hiding (Max (..), Min (..))

import           Control.Applicative   (liftA2)
import           Data.Coerce           (coerce)
import           GHC.Generics          (Generic, Generic1)

import           Data.Typeable         (Typeable)
import           Foreign.Storable      (Storable)

import           Data.Function         (fix)

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

class Semiring a => StarSemiring a where
  {-# MINIMAL star | plus #-}
  star :: a -> a
  plus :: a -> a
  star x = one <+> plus x
  plus x = x <.> star x

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------
instance Semiring Bool where
  one = True
  zero = False
  (<+>) = (||)
  (<.>) = (&&)

instance StarSemiring Bool where
  star _ = True
  plus = id

-- | Not lawful. Only for convenience.
instance Semiring a => Semiring (NegativeInfinite a) where
  one = pure one
  zero = pure zero
  (<+>) = liftA2 (<+>)
  (<.>) = liftA2 (<.>)

-- | Not lawful. Only for convenience.
instance Semiring a => Semiring (PositiveInfinite a) where
  one = pure one
  zero = pure zero
  (<+>) = liftA2 (<+>)
  (<.>) = liftA2 (<.>)

-- | Not lawful. Only for convenience.
instance Semiring a => Semiring (Infinite a) where
  one = pure one
  zero = pure zero
  (<+>) = (coerce :: (Infinite (Add a) -> Infinite (Add a) -> Infinite (Add a))
                  ->  Infinite      a  -> Infinite      a  -> Infinite      a
          ) mappend
  (<.>) = liftA2 (<.>)

instance (Eq a, Semiring a) => StarSemiring (PositiveInfinite a) where
  star (PosFinite x) | x == zero = one
  star _ = PositiveInfinity

instance Semiring () where
  one = ()
  zero = ()
  _ <+> _ = ()
  _ <.> _ = ()

instance StarSemiring () where
  star _ = ()
  plus _ = ()

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
             ,Enum, Typeable, Storable, Fractional, Real, RealFrac
             ,Functor, Foldable, Traversable, Semiring, StarSemiring)

-- | Monoid under '<.>'. Analogous to 'Data.Monoid.Product', but uses the
-- 'Semiring' constraint, rather than 'Num'.
newtype Mul a = Mul
  { getMul :: a
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Typeable, Storable, Fractional, Real, RealFrac
             ,Functor, Foldable, Traversable, Semiring, StarSemiring)

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

instance Semiring a => Semigroup (Add a) where
  (<>) = (coerce :: WrapBinary Add a) (<+>)

instance Semiring a => Semigroup (Mul a) where
  (<>) = (coerce :: WrapBinary Mul a) (<.>)

instance Semiring a => Monoid (Add a) where
  mempty = Add zero
  mappend = (<>)

instance Semiring a => Monoid (Mul a) where
  mempty = Mul one
  mappend = (<>)

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
add :: (Foldable f, Semiring a) => f a -> a
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
mul :: (Foldable f, Semiring a) => f a -> a
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
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Typeable, Storable, Fractional, Real, RealFrac
             ,Functor, Foldable, Traversable)

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
  } deriving (Eq, Ord, Read, Show, Bounded, Generic, Generic1, Num
             ,Enum, Typeable, Storable, Fractional, Real, RealFrac
             ,Functor, Foldable, Traversable)

instance Applicative Max where
  pure = Max
  (<*>) = (coerce :: (    (a -> b) ->     a ->     b)
                  ->  Max (a -> b) -> Max a -> Max b
          ) ($)

instance Applicative Min where
  pure = Min
  (<*>) = (coerce :: (    (a -> b) ->     a ->     b)
                  ->  Min (a -> b) -> Min a -> Min b
          ) ($)

instance Monad Max where Max x >>= f = f x
instance Monad Min where Min x >>= f = f x

instance Ord a => Semigroup (Max a) where
  (<>) = (coerce :: WrapBinary Max a) max

instance Ord a => Semigroup (Min a) where
  (<>) = (coerce :: WrapBinary Min a) min

-- | >>> (getMax . foldMap Max) [1..10]
-- 10.0
instance (Ord a, HasNegativeInfinity a) => Monoid (Max a) where
  mempty = Max negativeInfinity
  mappend = (<>)

-- | >>> (getMin . foldMap Min) [1..10]
-- 1.0
instance (Ord a, HasPositiveInfinity a) => Monoid (Min a) where
  mempty = Min positiveInfinity
  mappend = (<>)

instance (Semiring a, Ord a, HasNegativeInfinity a) => Semiring (Max a) where
  (<+>) = mappend
  zero = mempty
  (<.>) = (coerce :: WrapBinary Max a) (<+>)
  one = Max zero

instance (Semiring a, Ord a, HasPositiveInfinity a) => Semiring (Min a) where
  (<+>) = mappend
  zero = mempty
  (<.>) = (coerce :: WrapBinary Min a) (<+>)
  one = Min zero

instance (Semiring a, Ord a, HasPositiveInfinity a, HasNegativeInfinity a)
  => StarSemiring (Max a) where
    star (Max x) | x > zero = Max positiveInfinity
                | otherwise = Max zero

instance (Semiring a, Ord a, HasPositiveInfinity a, HasNegativeInfinity a)
  => StarSemiring (Min a) where
    star (Min x) | x < zero = Min negativeInfinity
                | otherwise = Min zero

------------------------------------------------------------------------
-- (->) instance
------------------------------------------------------------------------

-- | The @(->)@ instance is analogous to the one for 'Monoid'.
instance Semiring b => Semiring (a -> b) where
  zero = const zero
  one = const one
  (f <+> g) x = f x <+> g x
  (f <.> g) x = f x <.> g x

instance StarSemiring b => StarSemiring (a -> b) where
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
instance Monoid a => Semiring (Endo a) where
  zero = Endo mempty
  Endo f <+> Endo g = Endo (f `mappend` g)
  one = mempty
  (<.>) = mappend

instance (Monoid a, Eq a) => StarSemiring (Endo a) where
  star (Endo f) = Endo g where
    g x | x /= y = y `mappend` g y
        | y /= mempty = fix (mappend y)
        | otherwise = mempty
        where y = f x

------------------------------------------------------------------------
-- Instances for Bool wrappers
------------------------------------------------------------------------

instance Semiring Any where
  (<+>) = coerce (||)
  zero = Any False
  (<.>) = coerce (&&)
  one = Any True

instance StarSemiring Any where
  star _ = Any True
  plus = id

instance Semiring All where
  (<+>) = coerce (||)
  zero = All False
  (<.>) = coerce (&&)
  one = All True

instance StarSemiring All where
  star _ = All True
  plus = id

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
  zero  = $(repN 2 [| zero  |])
  (<+>) = $(cmbN 2 [| (<+>) |])
  one   = $(repN 2 [| one   |])
  (<.>) = $(cmbN 2 [| (<.>) |])

instance (Semiring a, Semiring b, Semiring c) => Semiring (a,b,c) where
  zero  = $(repN 3 [| zero  |])
  (<+>) = $(cmbN 3 [| (<+>) |])
  one   = $(repN 3 [| one   |])
  (<.>) = $(cmbN 3 [| (<.>) |])

instance (Semiring a, Semiring b, Semiring c, Semiring d) => Semiring (a,b,c,d) where
  zero  = $(repN 4 [| zero  |])
  (<+>) = $(cmbN 4 [| (<+>) |])
  one   = $(repN 4 [| one   |])
  (<.>) = $(cmbN 4 [| (<.>) |])

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e)
  => Semiring (a,b,c,d,e) where
    zero  = $(repN 5 [| zero  |])
    (<+>) = $(cmbN 5 [| (<+>) |])
    one   = $(repN 5 [| one   |])
    (<.>) = $(cmbN 5 [| (<.>) |])

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f)
  => Semiring (a,b,c,d,e,f) where
    zero  = $(repN 6 [| zero  |])
    (<+>) = $(cmbN 6 [| (<+>) |])
    one   = $(repN 6 [| one   |])
    (<.>) = $(cmbN 6 [| (<.>) |])

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f
         ,Semiring g)
  => Semiring (a,b,c,d,e,f,g) where
    zero  = $(repN 7 [| zero  |])
    (<+>) = $(cmbN 7 [| (<+>) |])
    one   = $(repN 7 [| one   |])
    (<.>) = $(cmbN 7 [| (<.>) |])

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f
         ,Semiring g, Semiring h)
  => Semiring (a,b,c,d,e,f,g,h) where
    zero  = $(repN 8 [| zero  |])
    (<+>) = $(cmbN 8 [| (<+>) |])
    one   = $(repN 8 [| one   |])
    (<.>) = $(cmbN 8 [| (<.>) |])

instance (Semiring a, Semiring b, Semiring c, Semiring d, Semiring e, Semiring f
         ,Semiring g, Semiring h, Semiring i)
  => Semiring (a,b,c,d,e,f,g,h,i) where
    zero  = $(repN 9 [| zero  |])
    (<+>) = $(cmbN 9 [| (<+>) |])
    one   = $(repN 9 [| one   |])
    (<.>) = $(cmbN 9 [| (<.>) |])

instance (StarSemiring a, StarSemiring b) => StarSemiring (a,b) where
  star = $(appN 2 [| star |])
  plus = $(appN 2 [| plus |])

instance (StarSemiring a, StarSemiring b, StarSemiring c)
  => StarSemiring (a,b,c) where
    star = $(appN 3 [| star |])
    plus = $(appN 3 [| plus |])

instance (StarSemiring a, StarSemiring b, StarSemiring c, StarSemiring d)
  => StarSemiring (a,b,c,d) where
    star = $(appN 4 [| star |])
    plus = $(appN 4 [| plus |])

instance (StarSemiring a, StarSemiring b, StarSemiring c, StarSemiring d
         ,StarSemiring e)
  => StarSemiring (a,b,c,d,e) where
    star = $(appN 5 [| star |])
    plus = $(appN 5 [| plus |])

instance (StarSemiring a, StarSemiring b, StarSemiring c, StarSemiring d
         ,StarSemiring e, StarSemiring f)
  => StarSemiring (a,b,c,d,e,f) where
    star = $(appN 6 [| star |])
    plus = $(appN 6 [| plus |])

instance (StarSemiring a, StarSemiring b, StarSemiring c, StarSemiring d
         ,StarSemiring e, StarSemiring f, StarSemiring g)
  => StarSemiring (a,b,c,d,e,f,g) where
    star = $(appN 7 [| star |])
    plus = $(appN 7 [| plus |])

instance (StarSemiring a, StarSemiring b, StarSemiring c, StarSemiring d
         ,StarSemiring e, StarSemiring f, StarSemiring g, StarSemiring h)
  => StarSemiring (a,b,c,d,e,f,g,h) where
    star = $(appN 8 [| star |])
    plus = $(appN 8 [| plus |])

instance (StarSemiring a, StarSemiring b, StarSemiring c, StarSemiring d
         ,StarSemiring e, StarSemiring f, StarSemiring g, StarSemiring h
         ,StarSemiring i)
  => StarSemiring (a,b,c,d,e,f,g,h,i) where
    star = $(appN 9 [| star |])
    plus = $(appN 9 [| plus |])
