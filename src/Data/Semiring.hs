{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

{-|
Module: Data.Semiring
Description: Haskell semirings
License: MIT
Maintainer: mail@doisinkidney.com
Stability: experimental
-}
module Data.Semiring
  (
   -- * Semiring classes
   Semiring(..)
  ,StarSemiring(..)
  ,mulFoldable
  ,addFoldable
  ,
   -- * Helper classes
   HasPositiveInfinity(..)
  ,HasNegativeInfinity(..)
  ,DetectableZero(..)
  ,
   -- * Monoidal wrappers
   Add(..)
  ,Mul(..)
  ,
   -- * Ordering wrappers
   Max(..)
  ,Min(..))
  where

import Data.Functor.Identity (Identity(..))
import Data.Complex (Complex)
import Data.Fixed (Fixed, HasResolution)
import Data.Ratio (Ratio)
import Numeric.Natural (Natural)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Word (Word16, Word32, Word64, Word8)
import Foreign.C.Types
       (CChar, CClock, CDouble, CFloat, CInt, CIntMax, CIntPtr, CLLong,
        CLong, CPtrdiff, CSChar, CSUSeconds, CShort, CSigAtomic, CSize,
        CTime, CUChar, CUInt, CUIntMax, CUIntPtr, CULLong, CULong,
        CUSeconds, CUShort, CWchar)
import Foreign.Ptr (IntPtr, WordPtr)
import System.Posix.Types
       (CCc, CDev, CGid, CIno, CMode, CNlink, COff, CPid, CRLim, CSpeed,
        CSsize, CTcflag, CUid, Fd)
import Data.Semigroup hiding (Max(..), Min(..))
import Data.Coerce (coerce)
import GHC.Generics (Generic, Generic1)
import Data.Typeable (Typeable)
import Foreign.Storable (Storable)
import Data.Semiring.TH

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Numeric.Log hiding (sum)
import qualified Numeric.Log

import GHC.Base (build)

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
--
-- @x '<=' y => x '<+>' z '<=' y '<+>' z
--x '<=' y => x '<+>' z '<=' y '<+>' z
--'zero' '<=' z '&&' x '<=' y => x '<.>' z '<=' y '<.>' z '&&' z '<.>' x '<=' z '<.>' y@
class Semiring a  where
    {-# MINIMAL zero , one , (<.>) , (<+>) #-}
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
        :: [a] -> a
    add = getAdd . foldMap Add
    {-# INLINE add #-}
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
        :: [a] -> a
    mul = getMul . foldMap Mul
    {-# INLINE mul #-}

mulFoldable :: (Foldable f, Semiring a) => f a -> a
mulFoldable xs = mul (build (\c n -> foldr c n xs))
{-# INLINE mulFoldable #-}

addFoldable :: (Foldable f, Semiring a) => f a -> a
addFoldable xs = add (build (\c n -> foldr c n xs))
{-# INLINE addFoldable #-}

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
    {-# INLINE star #-}
    plus x = x <.> star x
    {-# INLINE plus #-}

-- | Useful for operations where zeroes may need to be discarded: for instance
-- in sparse matrix calculations.
class Semiring a =>
      DetectableZero a  where
    -- | 'True' if x is 'zero'.
    isZero
        :: a -> Bool

isZeroEq
    :: (Semiring a, Eq a)
    => a -> Bool
isZeroEq = (zero ==)
{-# INLINE isZeroEq #-}

--------------------------------------------------------------------------------
-- Infinites
--------------------------------------------------------------------------------
-- | A class for semirings with a concept of "infinity". It's important that
-- this isn't regarded as the same as "bounded":
-- @x '<+>' 'positiveInfinity'@ should probably equal 'positiveInfinity'.
class HasPositiveInfinity a  where
    -- | A positive infinite value
    positiveInfinity
        :: a
    -- | Test if a value is positive infinity.
    isPositiveInfinity
        :: a -> Bool

defaultPositiveInfinity
    :: RealFloat a
    => a
defaultPositiveInfinity = 1 / 0
{-# INLINE defaultPositiveInfinity #-}

defaultIsPositiveInfinity
    :: RealFloat a
    => a -> Bool
defaultIsPositiveInfinity x = isInfinite x && x > 0
{-# INLINE defaultIsPositiveInfinity #-}

-- | A class for semirings with a concept of "negative infinity". It's important\
-- that this isn't regarded as the same as "bounded":
-- @x '<+>' 'negativeInfinity'@ should probably equal 'negativeInfinity'.
class HasNegativeInfinity a  where
    -- | A negative infinite value
    negativeInfinity
        :: a
    -- | Test if a value is negative infinity.
    isNegativeInfinity
        :: a -> Bool

defaultIsNegativeInfinity
    :: RealFloat a
    => a -> Bool
defaultIsNegativeInfinity x = isInfinite x && x < 0
{-# INLINE defaultIsNegativeInfinity #-}

defaultNegativeInfinity
    :: RealFloat a
    => a
defaultNegativeInfinity = negate (1 / 0)
{-# INLINE defaultNegativeInfinity #-}

instance HasPositiveInfinity Double where
    positiveInfinity = defaultPositiveInfinity
    isPositiveInfinity = defaultIsPositiveInfinity
    {-# INLINE positiveInfinity #-}
    {-# INLINE isPositiveInfinity #-}

instance HasNegativeInfinity Double where
    negativeInfinity = defaultNegativeInfinity
    isNegativeInfinity = defaultIsNegativeInfinity
    {-# INLINE negativeInfinity #-}
    {-# INLINE isNegativeInfinity #-}

instance HasPositiveInfinity Float where
    positiveInfinity = defaultPositiveInfinity
    isPositiveInfinity = defaultIsPositiveInfinity
    {-# INLINE positiveInfinity #-}
    {-# INLINE isPositiveInfinity #-}

instance HasNegativeInfinity Float where
    negativeInfinity = defaultNegativeInfinity
    isNegativeInfinity = defaultIsNegativeInfinity
    {-# INLINE negativeInfinity #-}
    {-# INLINE isNegativeInfinity #-}

instance HasPositiveInfinity CDouble where
    positiveInfinity = defaultPositiveInfinity
    isPositiveInfinity = defaultIsPositiveInfinity
    {-# INLINE positiveInfinity #-}
    {-# INLINE isPositiveInfinity #-}

instance HasNegativeInfinity CDouble where
    negativeInfinity = defaultNegativeInfinity
    isNegativeInfinity = defaultIsNegativeInfinity
    {-# INLINE negativeInfinity #-}
    {-# INLINE isNegativeInfinity #-}

instance HasPositiveInfinity CFloat where
    positiveInfinity = defaultPositiveInfinity
    isPositiveInfinity = defaultIsPositiveInfinity
    {-# INLINE positiveInfinity #-}
    {-# INLINE isPositiveInfinity #-}

instance HasNegativeInfinity CFloat where
    negativeInfinity = defaultNegativeInfinity
    isNegativeInfinity = defaultIsNegativeInfinity
    {-# INLINE negativeInfinity #-}
    {-# INLINE isNegativeInfinity #-}

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
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

instance DetectableZero Bool where
    isZero = not
    {-# INLINE isZero #-}

instance Semiring () where
    one = ()
    zero = ()
    _ <+> _ = ()
    _ <.> _ = ()
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance DetectableZero () where
    isZero _ = True
    {-# INLINE isZero #-}

instance StarSemiring () where
    star _ = ()
    plus _ = ()
    {-# INLINE star #-}
    {-# INLINE plus #-}

-- | A polynomial in /x/ can be defined as a list of its coefficients,
-- where the /i/th element is the coefficient of /x^i/. This is the
-- semiring for such a list. Adapted from
-- <https://pdfs.semanticscholar.org/702d/348c32133997e992db362a19697d5607ab32.pdf here>.
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

instance Semiring a =>
         DetectableZero [a] where
    isZero = null
    {-# INLINE isZero #-}

instance (Monoid a, Ord a) =>
         Semiring (Set a) where
    (<+>) = Set.union
    zero = Set.empty
    one = Set.singleton mempty
    xs <.> ys = foldMap (flip Set.map ys . mappend) xs
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}
    {-# INLINE zero #-}
    {-# INLINE one #-}

instance (Ord a, Monoid a, Semiring b) =>
         Semiring (Map a b) where
    one = Map.singleton mempty one
    {-# INLINE one #-}
    zero = Map.empty
    {-# INLINE zero #-}
    (<+>) = Map.unionWith (<+>)
    {-# INLINE (<+>) #-}
    xs <.> ys =
        Map.fromListWith
            (<+>)
            [ (mappend k l, v <.> u)
            | (k,v) <- Map.toList xs
            , (l,u) <- Map.toList ys ]
    {-# INLINE (<.>) #-}

instance (Monoid a, Ord a) =>
         DetectableZero (Set a) where
    isZero = Set.null
    {-# INLINE isZero #-}

instance (Precise a, RealFloat a) => Semiring (Log a) where
    (<.>) = (*)
    {-# INLINE (<.>) #-}
    (<+>) = (+)
    {-# INLINE (<+>) #-}
    one = Exp 0
    {-# INLINE one #-}
    zero = Exp (-(1/0))
    {-# INLINE zero #-}
    add = Numeric.Log.sum
    {-# INLINE add #-}

instance (Precise a, RealFloat a) => DetectableZero (Log a) where
    isZero = isZeroEq
    {-# INLINE isZero #-}

--------------------------------------------------------------------------------
-- Addition and multiplication newtypes
--------------------------------------------------------------------------------
type WrapBinary f a = (a -> a -> a) -> f a -> f a -> f a

-- | Monoid under '<+>'. Analogous to 'Data.Monoid.Sum', but uses the
-- 'Semiring' constraint, rather than 'Num'.
newtype Add a = Add
    { getAdd :: a
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,Semiring,DetectableZero,StarSemiring)

-- | Monoid under '<.>'. Analogous to 'Data.Monoid.Product', but uses the
-- 'Semiring' constraint, rather than 'Num'.
newtype Mul a = Mul
    { getMul :: a
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,Semiring,DetectableZero,StarSemiring)

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
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}
    mconcat = (coerce :: ([a] -> a) -> [Add a] -> Add a) add
    {-# INLINE mconcat #-}

instance Semiring a =>
         Monoid (Mul a) where
    mempty = Mul one
    {-# INLINE mempty #-}
    mappend = (<>)
    {-# INLINE mappend #-}
    mconcat = (coerce :: ([a] -> a) -> [Mul a] -> Mul a) mul
    {-# INLINE mconcat #-}

--------------------------------------------------------------------------------
-- Ord wrappers
--------------------------------------------------------------------------------
-- | The "<https://ncatlab.org/nlab/show/tropical+semiring Tropical>" or
-- min-plus semiring. It is a semiring where:
--
-- @'<+>'  = 'min'
--'zero' = ∞
--'<.>'  = '<+>'
--'one'  = 'zero'@
--
-- Note that we can't use 'Data.Semigroup.Min' from 'Data.Semigroup'
-- because annihilation needs to hold:
--
-- @∞ '<+>' x = x '<+>' ∞ = ∞@
--
-- Taking ∞ to be 'maxBound' would break the above law. Using 'positiveInfinity'
-- to represent it follows the law.
newtype Min a = Min
    { getMin :: a
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable)

-- | The "<https://ncatlab.org/nlab/show/max-plus+algebra Arctic>"
-- or max-plus semiring. It is a semiring where:
--
-- @'<+>'  = 'max'
--'zero' = -∞
--'<.>'  = '<+>'
--'one'  = 'zero'@
--
-- Note that we can't use 'Data.Semigroup.Max' from 'Data.Semigroup'
-- because annihilation needs to hold:
--
-- @-∞ '<+>' x = x '<+>' -∞ = -∞@
--
-- Taking -∞ to be 'minBound' would break the above law. Using
-- 'negativeInfinity' to represent it follows the law.
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

instance (Semiring a, Ord a, HasPositiveInfinity a) =>
         DetectableZero (Min a) where
    isZero (Min x) = isPositiveInfinity x
    {-# INLINE isZero #-}

instance (Semiring a, Ord a, HasNegativeInfinity a) =>
         DetectableZero (Max a) where
    isZero (Max x) = isNegativeInfinity x
    {-# INLINE isZero #-}

--------------------------------------------------------------------------------
-- (->) instance
--------------------------------------------------------------------------------
-- | The @(->)@ instance is analogous to the one for 'Monoid'.
instance Semiring b =>
         Semiring (a -> b) where
    zero = const zero
    {-# INLINE zero #-}
    one = const one
    {-# INLINE one #-}
    (f <+> g) x = f x <+> g x
    {-# INLINE (<+>) #-}
    (f <.> g) x = f x <.> g x
    {-# INLINE (<.>) #-}

instance StarSemiring b =>
         StarSemiring (a -> b) where
    star = (.) star
    {-# INLINE star #-}
    plus = (.) plus
    {-# INLINE plus #-}

--------------------------------------------------------------------------------
-- Endo instance
--------------------------------------------------------------------------------
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

instance (Enum a, Bounded a, Eq a, Monoid a) =>
         DetectableZero (Endo a) where
    isZero (Endo f) = all (mempty ==) (map f [minBound .. maxBound])

--------------------------------------------------------------------------------
-- Instances for Bool wrappers
--------------------------------------------------------------------------------
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

instance DetectableZero Any where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero All where
    isZero = isZeroEq
    {-# INLINE isZero #-}

--------------------------------------------------------------------------------
-- Boring instances
--------------------------------------------------------------------------------

instance Semiring Int where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Int8 where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Int16 where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Int32 where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Int64 where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Integer where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Word where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Word8 where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Word16 where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Word32 where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Word64 where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Float where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Double where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CUIntMax where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CIntMax where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CUIntPtr where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CIntPtr where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CSUSeconds where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CUSeconds where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CTime where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CClock where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CSigAtomic where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CWchar where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CSize where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CPtrdiff where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CDouble where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CFloat where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CULLong where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CLLong where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CULong where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CLong where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CUInt where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CInt where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CUShort where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CShort where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CUChar where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CSChar where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CChar where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring IntPtr where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring WordPtr where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Fd where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CRLim where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CTcflag where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CSpeed where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CCc where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CUid where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CNlink where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CGid where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CSsize where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CPid where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring COff where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CMode where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CIno where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring CDev where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring Natural where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Integral a =>
         Semiring (Ratio a) where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring a => Semiring (Product a) where
    one = Product one
    {-# INLINE one #-}
    zero = Product zero
    {-# INLINE zero #-}
    (<+>) = (coerce :: WrapBinary Product a) (<+>)
    {-# INLINE (<+>) #-}
    (<.>) = (coerce :: WrapBinary Product a) (<.>)
    {-# INLINE (<.>) #-}

instance Semiring a => Semiring (Sum a) where
    one = Sum one
    {-# INLINE one #-}
    zero = Sum zero
    {-# INLINE zero #-}
    (<+>) = (coerce :: WrapBinary Sum a) (<+>)
    {-# INLINE (<+>) #-}
    (<.>) = (coerce :: WrapBinary Sum a) (<.>)
    {-# INLINE (<.>) #-}

instance RealFloat a =>
         Semiring (Complex a) where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance HasResolution a =>
         Semiring (Fixed a) where
    one = 1
    zero = 0
    (<+>) = (+)
    (<.>) = (*)
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance Semiring a => Semiring (Identity a) where
    one = Identity one
    {-# INLINE one #-}
    zero = Identity zero
    {-# INLINE zero #-}
    (<+>) = (coerce :: WrapBinary Identity a) (<+>)
    {-# INLINE (<+>) #-}
    (<.>) = (coerce :: WrapBinary Identity a) (<.>)
    {-# INLINE (<.>) #-}

instance DetectableZero Int where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Int8 where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Int16 where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Int32 where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Int64 where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Integer where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Word where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Word8 where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Word16 where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Word32 where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Word64 where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Float where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Double where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CUIntMax where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CIntMax where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CUIntPtr where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CIntPtr where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CSUSeconds where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CUSeconds where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CTime where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CClock where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CSigAtomic where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CWchar where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CSize where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CPtrdiff where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CDouble where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CFloat where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CULLong where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CLLong where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CULong where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CLong where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CUInt where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CInt where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CUShort where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CShort where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CUChar where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CSChar where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CChar where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero IntPtr where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero WordPtr where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Fd where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CRLim where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CTcflag where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CSpeed where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CCc where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CUid where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CNlink where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CGid where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CSsize where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CPid where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero COff where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CMode where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CIno where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero CDev where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance DetectableZero Natural where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance Integral a =>
         DetectableZero (Ratio a) where
    isZero = isZeroEq
    {-# INLINE isZero #-}

deriving instance DetectableZero a => DetectableZero (Product a)

deriving instance DetectableZero a => DetectableZero (Sum a)

instance RealFloat a =>
         DetectableZero (Complex a) where
    isZero = isZeroEq
    {-# INLINE isZero #-}

instance HasResolution a =>
         DetectableZero (Fixed a) where
    isZero = isZeroEq
    {-# INLINE isZero #-}

deriving instance DetectableZero a => DetectableZero (Identity a)

--------------------------------------------------------------------------------
-- Very boring instances
--------------------------------------------------------------------------------
$(traverse semiringIns [2 .. 9])

$(traverse starIns [2 .. 9])

$(traverse zeroIns [2 .. 9])
