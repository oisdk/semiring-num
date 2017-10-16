{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

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
  , PosFrac(..)
  , PosInt(..)
  ) where

import           Data.Coerce

import           Data.Semiring

import           GHC.Generics     (Generic,Generic1)
import           Data.Typeable    (Typeable)
import           Foreign.Storable (Storable)
import           Data.Functor.Classes

import           Data.Semiring.Newtype

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed.Base    as U

import           Control.DeepSeq

-- | Useful for some constraint problems.
--
-- @('<+>') = 'max'
--('<.>') = 'min'
--'zero'  = 'minBound'
--'one'   = 'maxBound'@
newtype Bottleneck a = Bottleneck
    { getBottleneck :: a
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,NFData)

instance (Bounded a, Ord a) => Semiring (Bottleneck a) where
  (<+>) = (coerce :: WrapBinary Bottleneck a) max
  (<.>) = (coerce :: WrapBinary Bottleneck a) min
  zero = Bottleneck minBound
  one  = Bottleneck maxBound
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
  {-# INLINE zero #-}
  {-# INLINE one #-}

instance (Bounded a, Ord a) => DetectableZero (Bottleneck a) where
  isZero = (zero==)

instance Eq1 Bottleneck where
    liftEq = coerce

instance Ord1 Bottleneck where
    liftCompare = coerce

instance Show1 Bottleneck where
    liftShowsPrec = showsNewtype "Bottleneck" "getBottleneck"

instance Read1 Bottleneck where
    liftReadsPrec = readsNewtype "Bottleneck" "getBottleneck"

-- | Positive numbers only.
--
-- @('<+>') = 'gcd'
--('<.>') = 'lcm'
--'zero'  = 'zero'
--'one'   = 'one'@
newtype Division a = Division
    { getDivision :: a
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,DetectableZero,NFData)

-- | Only expects positive numbers
instance (Integral a, Semiring a) => Semiring (Division a) where
  (<+>) = (coerce :: WrapBinary Division a) gcd
  (<.>) = (coerce :: WrapBinary Division a) lcm
  zero = Division zero
  one = Division one
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
  {-# INLINE zero #-}
  {-# INLINE one #-}

instance Eq1 Division where
    liftEq = coerce

instance Ord1 Division where
    liftCompare = coerce

instance Show1 Division where
    liftShowsPrec = showsNewtype "Division" "getDivision"

instance Read1 Division where
    liftReadsPrec = readsNewtype "Division" "getDivision"

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
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,NFData)

instance (Ord a, Num a) => Semiring (Łukasiewicz a) where
  (<+>) = (coerce :: WrapBinary Łukasiewicz a) max
  (<.>) = (coerce :: WrapBinary Łukasiewicz a) (\x y -> max 0 (x + y - 1))
  zero = Łukasiewicz 0
  one  = Łukasiewicz 1
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
  {-# INLINE zero #-}
  {-# INLINE one #-}

instance (Ord a, Num a) => DetectableZero (Łukasiewicz a) where
  isZero = (zero==)

instance Eq1 Łukasiewicz where
    liftEq = coerce

instance Ord1 Łukasiewicz where
    liftCompare = coerce

instance Show1 Łukasiewicz where
    liftShowsPrec = showsNewtype "Łukasiewicz" "getŁukasiewicz"

instance Read1 Łukasiewicz where
    liftReadsPrec = readsNewtype "Łukasiewicz" "getŁukasiewicz"

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
    } deriving (Eq,Ord,Read,Show,Bounded,Generic,Generic1,Num,Enum,Typeable
               ,Storable,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,DetectableZero,NFData)

instance (Ord a, Semiring a) => Semiring (Viterbi a) where
  (<+>) = (coerce :: WrapBinary Viterbi a) max
  (<.>) = (coerce :: WrapBinary Viterbi a) (<.>)
  zero = Viterbi zero
  one  = Viterbi one
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
  {-# INLINE zero #-}
  {-# INLINE one #-}

instance Eq1 Viterbi where
    liftEq = coerce

instance Ord1 Viterbi where
    liftCompare = coerce

instance Show1 Viterbi where
    liftShowsPrec = showsNewtype "Viterbi" "getViterbi"

instance Read1 Viterbi where
    liftReadsPrec = readsNewtype "Viterbi" "getViterbi"

-- | Adds a star operation to fractional types.
--
-- @('<+>')  = ('<+>')
--('<.>')  = ('<.>')
--'zero'   = 'zero'
--'one'    = 'one'
--'star' x = if x < 1 then 1 / (1 - x) else 'positiveInfinity'@
newtype PosFrac a = PosFrac
    { getPosFrac :: a
    } deriving (Eq,Ord,Read,Show,Generic,Generic1,Num,Enum,Typeable,Storable
               ,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,DetectableZero,NFData)

instance (Bounded a, Semiring a) => Bounded (PosFrac a) where
  minBound = PosFrac zero
  maxBound = PosFrac maxBound

instance Semiring a => Semiring (PosFrac a) where
  (<+>) = (coerce :: WrapBinary PosFrac a) (<+>)
  (<.>) = (coerce :: WrapBinary PosFrac a) (<.>)
  zero = PosFrac zero
  one  = PosFrac one
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
  {-# INLINE zero #-}
  {-# INLINE one #-}

instance (Ord a, Fractional a, Semiring a, HasPositiveInfinity a) =>
         StarSemiring (PosFrac a) where
    star (PosFrac n)
      | n < 1 = PosFrac (1 / (1 - n))
      | otherwise = PosFrac positiveInfinity

instance Eq1 PosFrac where
    liftEq = coerce

instance Ord1 PosFrac where
    liftCompare = coerce

instance Show1 PosFrac where
    liftShowsPrec = showsNewtype "PosFrac" "getPosFrac"

instance Read1 PosFrac where
    liftReadsPrec = readsNewtype "PosFrac" "getPosFrac"

-- | Adds a star operation to integral types.
--
-- @('<+>')  = ('<+>')
--('<.>')  = ('<.>')
--'zero'   = 'zero'
--'one'    = 'one'
--'star' 0 = 1
--'star' _ = 'positiveInfinity'@
newtype PosInt a = PosInt
    { getPosInt :: a
    } deriving (Eq,Ord,Read,Show,Generic,Generic1,Num,Enum,Typeable,Storable
               ,Fractional,Real,RealFrac,Functor,Foldable,Traversable
               ,DetectableZero,NFData)

instance (Bounded a, Semiring a) => Bounded (PosInt a) where
  minBound = PosInt zero
  maxBound = PosInt maxBound

instance Semiring a => Semiring (PosInt a) where
  (<+>) = (coerce :: WrapBinary PosInt a) (<+>)
  (<.>) = (coerce :: WrapBinary PosInt a) (<.>)
  zero = PosInt zero
  one  = PosInt one
  {-# INLINE (<+>) #-}
  {-# INLINE (<.>) #-}
  {-# INLINE zero #-}
  {-# INLINE one #-}

instance (Eq a, Semiring a, HasPositiveInfinity a) =>
         StarSemiring (PosInt a) where
    star (PosInt n) | n == zero = PosInt one
    star _          = PosInt positiveInfinity

instance Eq1 PosInt where
    liftEq = coerce

instance Ord1 PosInt where
    liftCompare = coerce

instance Show1 PosInt where
    liftShowsPrec = showsNewtype "PosInt" "getPosInt"

instance Read1 PosInt where
    liftReadsPrec = readsNewtype "PosInt" "getPosInt"

newtype instance U.Vector (Bottleneck a) = V_Bottleneck (U.Vector a)
newtype instance U.MVector s (Bottleneck a) = MV_Bottleneck (U.MVector s a)

instance U.Unbox a =>
         M.MVector U.MVector (Bottleneck a) where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength =
        (coerce :: (U.MVector s a -> Int) -> U.MVector s (Bottleneck a) -> Int)
            M.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.MVector s a -> U.MVector s a) -> Int -> Int -> U.MVector s (Bottleneck a) -> U.MVector s (Bottleneck a))
            M.basicUnsafeSlice
    basicOverlaps =
        (coerce :: (U.MVector s a -> U.MVector s a -> Bool) -> U.MVector s (Bottleneck a) -> U.MVector s (Bottleneck a) -> Bool)
            M.basicOverlaps
    basicUnsafeNew n =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (Bottleneck a))
            (M.basicUnsafeNew n)
    basicUnsafeRead (MV_Bottleneck xs) i =
        fmap (coerce :: a -> Bottleneck a) (M.basicUnsafeRead xs i)
    basicUnsafeWrite =
        (coerce :: (U.MVector s a -> Int -> a -> m ()) -> U.MVector s (Bottleneck a) -> Int -> Bottleneck a -> m ())
            M.basicUnsafeWrite
    basicInitialize =
        (coerce :: (U.MVector s a -> m ()) -> U.MVector s (Bottleneck a) -> m ())
            M.basicInitialize

instance U.Unbox a =>
         G.Vector U.Vector (Bottleneck a) where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_Bottleneck xs) =
        fmap
            (coerce :: U.Vector a -> U.Vector (Bottleneck a))
            (G.basicUnsafeFreeze xs)
    basicUnsafeThaw (V_Bottleneck xs) =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (Bottleneck a))
            (G.basicUnsafeThaw xs)
    basicLength =
        (coerce :: (U.Vector a -> Int) -> U.Vector (Bottleneck a) -> Int)
            G.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.Vector a -> U.Vector a) -> Int -> Int -> U.Vector (Bottleneck a) -> U.Vector (Bottleneck a))
            G.basicUnsafeSlice
    basicUnsafeIndexM (V_Bottleneck xs) i =
        fmap (coerce :: a -> Bottleneck a) (G.basicUnsafeIndexM xs i)

newtype instance U.Vector (Division a) = V_Division (U.Vector a)
newtype instance U.MVector s (Division a) = MV_Division (U.MVector s a)

instance U.Unbox a =>
         M.MVector U.MVector (Division a) where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength =
        (coerce :: (U.MVector s a -> Int) -> U.MVector s (Division a) -> Int)
            M.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.MVector s a -> U.MVector s a) -> Int -> Int -> U.MVector s (Division a) -> U.MVector s (Division a))
            M.basicUnsafeSlice
    basicOverlaps =
        (coerce :: (U.MVector s a -> U.MVector s a -> Bool) -> U.MVector s (Division a) -> U.MVector s (Division a) -> Bool)
            M.basicOverlaps
    basicUnsafeNew n =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (Division a))
            (M.basicUnsafeNew n)
    basicUnsafeRead (MV_Division xs) i =
        fmap (coerce :: a -> Division a) (M.basicUnsafeRead xs i)
    basicUnsafeWrite =
        (coerce :: (U.MVector s a -> Int -> a -> m ()) -> U.MVector s (Division a) -> Int -> Division a -> m ())
            M.basicUnsafeWrite
    basicInitialize =
        (coerce :: (U.MVector s a -> m ()) -> U.MVector s (Division a) -> m ())
            M.basicInitialize

instance U.Unbox a =>
         G.Vector U.Vector (Division a) where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_Division xs) =
        fmap
            (coerce :: U.Vector a -> U.Vector (Division a))
            (G.basicUnsafeFreeze xs)
    basicUnsafeThaw (V_Division xs) =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (Division a))
            (G.basicUnsafeThaw xs)
    basicLength =
        (coerce :: (U.Vector a -> Int) -> U.Vector (Division a) -> Int)
            G.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.Vector a -> U.Vector a) -> Int -> Int -> U.Vector (Division a) -> U.Vector (Division a))
            G.basicUnsafeSlice
    basicUnsafeIndexM (V_Division xs) i =
        fmap (coerce :: a -> Division a) (G.basicUnsafeIndexM xs i)

newtype instance U.Vector (Łukasiewicz a) = V_Łukasiewicz (U.Vector a)
newtype instance U.MVector s (Łukasiewicz a) = MV_Łukasiewicz (U.MVector s a)

instance U.Unbox a =>
         M.MVector U.MVector (Łukasiewicz a) where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength =
        (coerce :: (U.MVector s a -> Int) -> U.MVector s (Łukasiewicz a) -> Int)
            M.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.MVector s a -> U.MVector s a) -> Int -> Int -> U.MVector s (Łukasiewicz a) -> U.MVector s (Łukasiewicz a))
            M.basicUnsafeSlice
    basicOverlaps =
        (coerce :: (U.MVector s a -> U.MVector s a -> Bool) -> U.MVector s (Łukasiewicz a) -> U.MVector s (Łukasiewicz a) -> Bool)
            M.basicOverlaps
    basicUnsafeNew n =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (Łukasiewicz a))
            (M.basicUnsafeNew n)
    basicUnsafeRead (MV_Łukasiewicz xs) i =
        fmap (coerce :: a -> Łukasiewicz a) (M.basicUnsafeRead xs i)
    basicUnsafeWrite =
        (coerce :: (U.MVector s a -> Int -> a -> m ()) -> U.MVector s (Łukasiewicz a) -> Int -> Łukasiewicz a -> m ())
            M.basicUnsafeWrite
    basicInitialize =
        (coerce :: (U.MVector s a -> m ()) -> U.MVector s (Łukasiewicz a) -> m ())
            M.basicInitialize

instance U.Unbox a =>
         G.Vector U.Vector (Łukasiewicz a) where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_Łukasiewicz xs) =
        fmap
            (coerce :: U.Vector a -> U.Vector (Łukasiewicz a))
            (G.basicUnsafeFreeze xs)
    basicUnsafeThaw (V_Łukasiewicz xs) =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (Łukasiewicz a))
            (G.basicUnsafeThaw xs)
    basicLength =
        (coerce :: (U.Vector a -> Int) -> U.Vector (Łukasiewicz a) -> Int)
            G.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.Vector a -> U.Vector a) -> Int -> Int -> U.Vector (Łukasiewicz a) -> U.Vector (Łukasiewicz a))
            G.basicUnsafeSlice
    basicUnsafeIndexM (V_Łukasiewicz xs) i =
        fmap (coerce :: a -> Łukasiewicz a) (G.basicUnsafeIndexM xs i)

newtype instance U.Vector (Viterbi a) = V_Viterbi (U.Vector a)
newtype instance U.MVector s (Viterbi a) = MV_Viterbi (U.MVector s a)

instance U.Unbox a =>
         M.MVector U.MVector (Viterbi a) where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength =
        (coerce :: (U.MVector s a -> Int) -> U.MVector s (Viterbi a) -> Int)
            M.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.MVector s a -> U.MVector s a) -> Int -> Int -> U.MVector s (Viterbi a) -> U.MVector s (Viterbi a))
            M.basicUnsafeSlice
    basicOverlaps =
        (coerce :: (U.MVector s a -> U.MVector s a -> Bool) -> U.MVector s (Viterbi a) -> U.MVector s (Viterbi a) -> Bool)
            M.basicOverlaps
    basicUnsafeNew n =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (Viterbi a))
            (M.basicUnsafeNew n)
    basicUnsafeRead (MV_Viterbi xs) i =
        fmap (coerce :: a -> Viterbi a) (M.basicUnsafeRead xs i)
    basicUnsafeWrite =
        (coerce :: (U.MVector s a -> Int -> a -> m ()) -> U.MVector s (Viterbi a) -> Int -> Viterbi a -> m ())
            M.basicUnsafeWrite
    basicInitialize =
        (coerce :: (U.MVector s a -> m ()) -> U.MVector s (Viterbi a) -> m ())
            M.basicInitialize

instance U.Unbox a =>
         G.Vector U.Vector (Viterbi a) where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_Viterbi xs) =
        fmap
            (coerce :: U.Vector a -> U.Vector (Viterbi a))
            (G.basicUnsafeFreeze xs)
    basicUnsafeThaw (V_Viterbi xs) =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (Viterbi a))
            (G.basicUnsafeThaw xs)
    basicLength =
        (coerce :: (U.Vector a -> Int) -> U.Vector (Viterbi a) -> Int)
            G.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.Vector a -> U.Vector a) -> Int -> Int -> U.Vector (Viterbi a) -> U.Vector (Viterbi a))
            G.basicUnsafeSlice
    basicUnsafeIndexM (V_Viterbi xs) i =
        fmap (coerce :: a -> Viterbi a) (G.basicUnsafeIndexM xs i)

newtype instance U.Vector (PosFrac a) = V_PosFrac (U.Vector a)
newtype instance U.MVector s (PosFrac a) = MV_PosFrac (U.MVector s a)

instance U.Unbox a =>
         M.MVector U.MVector (PosFrac a) where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength =
        (coerce :: (U.MVector s a -> Int) -> U.MVector s (PosFrac a) -> Int)
            M.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.MVector s a -> U.MVector s a) -> Int -> Int -> U.MVector s (PosFrac a) -> U.MVector s (PosFrac a))
            M.basicUnsafeSlice
    basicOverlaps =
        (coerce :: (U.MVector s a -> U.MVector s a -> Bool) -> U.MVector s (PosFrac a) -> U.MVector s (PosFrac a) -> Bool)
            M.basicOverlaps
    basicUnsafeNew n =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (PosFrac a))
            (M.basicUnsafeNew n)
    basicUnsafeRead (MV_PosFrac xs) i =
        fmap (coerce :: a -> PosFrac a) (M.basicUnsafeRead xs i)
    basicUnsafeWrite =
        (coerce :: (U.MVector s a -> Int -> a -> m ()) -> U.MVector s (PosFrac a) -> Int -> PosFrac a -> m ())
            M.basicUnsafeWrite
    basicInitialize =
        (coerce :: (U.MVector s a -> m ()) -> U.MVector s (PosFrac a) -> m ())
            M.basicInitialize

instance U.Unbox a =>
         G.Vector U.Vector (PosFrac a) where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_PosFrac xs) =
        fmap
            (coerce :: U.Vector a -> U.Vector (PosFrac a))
            (G.basicUnsafeFreeze xs)
    basicUnsafeThaw (V_PosFrac xs) =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (PosFrac a))
            (G.basicUnsafeThaw xs)
    basicLength =
        (coerce :: (U.Vector a -> Int) -> U.Vector (PosFrac a) -> Int)
            G.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.Vector a -> U.Vector a) -> Int -> Int -> U.Vector (PosFrac a) -> U.Vector (PosFrac a))
            G.basicUnsafeSlice
    basicUnsafeIndexM (V_PosFrac xs) i =
        fmap (coerce :: a -> PosFrac a) (G.basicUnsafeIndexM xs i)

newtype instance U.Vector (PosInt a) = V_PosInt (U.Vector a)
newtype instance U.MVector s (PosInt a) = MV_PosInt (U.MVector s a)

instance U.Unbox a =>
         M.MVector U.MVector (PosInt a) where
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicOverlaps #-}
    {-# INLINE basicUnsafeNew #-}
    {-# INLINE basicUnsafeRead #-}
    {-# INLINE basicUnsafeWrite #-}
    basicLength =
        (coerce :: (U.MVector s a -> Int) -> U.MVector s (PosInt a) -> Int)
            M.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.MVector s a -> U.MVector s a) -> Int -> Int -> U.MVector s (PosInt a) -> U.MVector s (PosInt a))
            M.basicUnsafeSlice
    basicOverlaps =
        (coerce :: (U.MVector s a -> U.MVector s a -> Bool) -> U.MVector s (PosInt a) -> U.MVector s (PosInt a) -> Bool)
            M.basicOverlaps
    basicUnsafeNew n =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (PosInt a))
            (M.basicUnsafeNew n)
    basicUnsafeRead (MV_PosInt xs) i =
        fmap (coerce :: a -> PosInt a) (M.basicUnsafeRead xs i)
    basicUnsafeWrite =
        (coerce :: (U.MVector s a -> Int -> a -> m ()) -> U.MVector s (PosInt a) -> Int -> PosInt a -> m ())
            M.basicUnsafeWrite
    basicInitialize =
        (coerce :: (U.MVector s a -> m ()) -> U.MVector s (PosInt a) -> m ())
            M.basicInitialize

instance U.Unbox a =>
         G.Vector U.Vector (PosInt a) where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw #-}
    {-# INLINE basicLength #-}
    {-# INLINE basicUnsafeSlice #-}
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeFreeze (MV_PosInt xs) =
        fmap
            (coerce :: U.Vector a -> U.Vector (PosInt a))
            (G.basicUnsafeFreeze xs)
    basicUnsafeThaw (V_PosInt xs) =
        fmap
            (coerce :: U.MVector s a -> U.MVector s (PosInt a))
            (G.basicUnsafeThaw xs)
    basicLength =
        (coerce :: (U.Vector a -> Int) -> U.Vector (PosInt a) -> Int)
            G.basicLength
    basicUnsafeSlice =
        (coerce :: (Int -> Int -> U.Vector a -> U.Vector a) -> Int -> Int -> U.Vector (PosInt a) -> U.Vector (PosInt a))
            G.basicUnsafeSlice
    basicUnsafeIndexM (V_PosInt xs) i =
        fmap (coerce :: a -> PosInt a) (G.basicUnsafeIndexM xs i)
