{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- | This module provides various "infinite" wrappers, which can provide
-- a detectable infinity to an otherwise non-infinite type.
module Data.Semiring.Infinite
  ( HasPositiveInfinity(..)
  , HasNegativeInfinity(..)
  , NegativeInfinite(..)
  , PositiveInfinite(..)
  , Infinite(..)
  ) where

import           Control.Applicative         (liftA2)
import           Data.Typeable               (Typeable)
import           GHC.Generics                (Generic, Generic1)

import           Data.Word                   (Word8)
import           Foreign.Ptr                 (Ptr, castPtr)
import           Foreign.Storable            (Storable, alignment, peek,
                                              peekByteOff, poke, pokeByteOff,
                                              sizeOf)

import           Data.Coerce
import           Data.Monoid
import           Data.Bool

import           Data.Semiring

import           Data.Semiring.Newtype

import           Control.DeepSeq

import           Data.Functor.Classes
import           Text.Read

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.Vector.Unboxed.Base    as U

-- | Adds negative infinity to a type. Useful for expressing detectable infinity
-- in types like 'Integer', etc.
data NegativeInfinite a
  = NegativeInfinity
  | NegFinite !a
  deriving (Eq, Ord, Read, Show, Generic, Generic1, Typeable, Functor
           ,Foldable, Traversable)

-- | Adds positive infinity to a type. Useful for expressing detectable infinity
-- in types like 'Integer', etc.
data PositiveInfinite a
  = PosFinite !a
  | PositiveInfinity
  deriving (Eq, Ord, Read, Show, Generic, Generic1, Typeable, Functor
           ,Foldable, Traversable)

instance Applicative NegativeInfinite where
  pure = NegFinite
  {-# INLINE pure #-}
  NegFinite f <*> NegFinite x = NegFinite (f x)
  _ <*> _ = NegativeInfinity
  {-# INLINE (<*>) #-}

instance Applicative PositiveInfinite where
  pure = PosFinite
  {-# INLINE pure #-}
  PosFinite f <*> PosFinite x = PosFinite (f x)
  _ <*> _ = PositiveInfinity
  {-# INLINE (<*>) #-}

-- | Adds positive and negative infinity to a type. Useful for expressing
-- detectable infinity in types like 'Integer', etc.
data Infinite a
  = Negative
  | Finite !a
  | Positive
  deriving (Eq, Ord, Read, Show, Generic, Generic1, Typeable, Functor
           ,Foldable, Traversable)

-- | Doesn't follow 'Test.Semiring.annihilateL' or 'Test.Semiring.mulDistribR'.
instance DetectableZero a =>
         Semiring (NegativeInfinite a) where
    one = pure one
    zero = pure zero
    (<+>) =
        (coerce :: CoerceBinary (NegativeInfinite (Add a)) (NegativeInfinite a))
            mappend
    x <.> y | any isZero x = zero
            | otherwise = liftA2 (<.>) x y
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

-- | Only lawful when used with positive numbers.
instance DetectableZero a =>
         Semiring (PositiveInfinite a) where
    one = pure one
    zero = pure zero
    (<+>) =
        (coerce :: CoerceBinary (PositiveInfinite (Add a)) (PositiveInfinite a))
            mappend
    x <.> y
      | any isZero x || any isZero y = zero
      | otherwise = liftA2 (<.>) x y
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

-- | Not distributive.
instance (DetectableZero a, Ord a) =>
         Semiring (Infinite a) where
    one = pure one
    zero = pure zero
    (<+>) = (coerce :: CoerceBinary (Infinite (Add a)) (Infinite a)) mappend
    Finite x <.> Finite y = Finite (x <.> y)
    Finite x <.> Negative = case compare x zero of
      LT -> Positive
      EQ -> zero
      GT -> Negative
    Finite x <.> Positive = case compare x zero of
      LT -> Negative
      EQ -> zero
      GT -> Positive
    Negative <.> Finite y = case compare y zero of
      LT -> Positive
      EQ -> zero
      GT -> Negative
    Positive <.> Finite y = case compare y zero of
      LT -> Negative
      EQ -> zero
      GT -> Positive
    Negative <.> Negative = Positive
    Negative <.> Positive = Negative
    Positive <.> Negative = Negative
    Positive <.> Positive = Positive
    {-# INLINE zero #-}
    {-# INLINE one #-}
    {-# INLINE (<+>) #-}
    {-# INLINE (<.>) #-}

instance (DetectableZero a) =>
         StarSemiring (PositiveInfinite a) where
    star (PosFinite x)
      | isZero x = one
    star _ = PositiveInfinity

instance DetectableZero a =>
         DetectableZero (NegativeInfinite a) where
    isZero = any isZero

instance DetectableZero a =>
         DetectableZero (PositiveInfinite a) where
    isZero = any isZero

instance (DetectableZero a, Ord a) =>
         DetectableZero (Infinite a) where
    isZero = any isZero

instance Applicative Infinite where
  pure = Finite
  {-# INLINE pure #-}
  Finite f <*> Finite x = Finite (f x)
  Negative <*> Negative = Positive
  Negative <*> _ = Negative
  _ <*> Negative = Negative
  _ <*> _ = Positive
  {-# INLINE (<*>) #-}

instance Bounded a => Bounded (NegativeInfinite a) where
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}
  minBound = NegativeInfinity
  maxBound = pure maxBound

instance Bounded a => Bounded (PositiveInfinite a) where
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}
  minBound = pure minBound
  maxBound = PositiveInfinity

instance Bounded (Infinite a) where
  {-# INLINE minBound #-}
  {-# INLINE maxBound #-}
  minBound = Negative
  maxBound = Positive

instance HasNegativeInfinity (NegativeInfinite a) where
  {-# INLINE negativeInfinity #-}
  negativeInfinity = NegativeInfinity
  isNegativeInfinity NegativeInfinity = True
  isNegativeInfinity _                = False

instance HasPositiveInfinity (PositiveInfinite a) where
  {-# INLINE positiveInfinity #-}
  positiveInfinity = PositiveInfinity
  isPositiveInfinity PositiveInfinity = True
  isPositiveInfinity _                = False

instance HasNegativeInfinity (Infinite a) where
  {-# INLINE negativeInfinity #-}
  negativeInfinity = Negative
  isNegativeInfinity Negative = True
  isNegativeInfinity _        = False

instance HasPositiveInfinity (Infinite a) where
  {-# INLINE positiveInfinity #-}
  positiveInfinity = Positive
  isPositiveInfinity Positive = True
  isPositiveInfinity _        = False

instance (Enum a, Bounded a, Eq a) => Enum (NegativeInfinite a) where
  succ = foldr (const . pure . succ) (pure minBound)
  pred NegativeInfinity = error "Predecessor of negative infinity"
  pred (NegFinite x) | x == minBound = NegativeInfinity
                     | otherwise = NegFinite (pred x)
  toEnum 0 = NegativeInfinity
  toEnum n = NegFinite (toEnum (n-1))
  fromEnum = foldr (const . succ . fromEnum) 0
  enumFrom NegativeInfinity = NegativeInfinity : map pure [minBound..]
  enumFrom (NegFinite x)    = map pure [x..]

maxBoundOf :: Bounded a => f a -> a
maxBoundOf _ = maxBound

instance (Enum a, Bounded a, Eq a) => Enum (PositiveInfinite a) where
  pred = foldr (const . pure . pred) (pure maxBound)
  succ PositiveInfinity = error "Successor of positive infinity"
  succ (PosFinite x) | x == maxBound = PositiveInfinity
                     | otherwise = PosFinite (succ x)
  toEnum n
    | n == toEnum (maxBoundOf PositiveInfinity) + 1 = PositiveInfinity
    | otherwise = PosFinite (toEnum n)
  fromEnum p@PositiveInfinity = fromEnum (maxBoundOf p) + 1
  fromEnum (PosFinite x)      = fromEnum x
  enumFrom PositiveInfinity = [PositiveInfinity]
  enumFrom (PosFinite x)    = map pure [x..] ++ [PositiveInfinity]

instance (Enum a, Bounded a, Eq a) => Enum (Infinite a) where
  pred Negative = error "Predecessor of negative infinity"
  pred Positive = Finite maxBound
  pred (Finite x) | x == minBound = Negative
                  | otherwise = Finite (pred x)
  succ Negative = Finite minBound
  succ Positive = error "Successor of positive infinity"
  succ (Finite x) | x == maxBound = Positive
                  | otherwise = Finite (succ x)
  toEnum 0 = Negative
  toEnum n | n == toEnum (maxBoundOf Positive) + 2 = Positive
           | otherwise = Finite (toEnum (n-1))
  fromEnum Negative   = 0
  fromEnum (Finite x) = fromEnum x + 1
  fromEnum p@Positive = fromEnum (maxBoundOf p) + 1
  enumFrom Positive   = [Positive]
  enumFrom Negative   = Negative : map pure [minBound..] ++ [Positive]
  enumFrom (Finite x) = map pure (enumFrom x) ++ [Positive]

instance Monoid a => Monoid (NegativeInfinite a) where
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
  mempty = pure mempty
  mappend = liftA2 mappend

instance Monoid a => Monoid (PositiveInfinite a) where
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
  mempty = pure mempty
  mappend = liftA2 mappend

instance Monoid a => Monoid (Infinite a) where
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}
  mempty = pure mempty
  Negative `mappend` Positive = Positive
  Positive `mappend` Negative = Positive
  Finite x `mappend` Finite y = pure (x `mappend` y)
  Negative `mappend` _ = Negative
  Positive `mappend` _ = Positive
  _ `mappend` y = y

instance Num a => Num (NegativeInfinite a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = foldr (const . pure . signum) (-1)
  (-) = liftA2 (-)

instance Num a => Num (PositiveInfinite a) where
  fromInteger = pure . fromInteger
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  abs = fmap abs
  signum = foldr (const . pure . signum) (-1)
  (-) = liftA2 (-)

instance Num a => Num (Infinite a) where
  fromInteger = Finite . fromInteger
  (+) = (coerce :: CoerceBinary (Infinite (Sum a)) (Infinite a)) mappend
  (*) = liftA2 (*)
  signum Positive   = 1
  signum Negative   = -1
  signum (Finite x) = Finite (signum x)
  negate Positive   = Negative
  negate Negative   = Positive
  negate (Finite x) = Finite (negate x)
  abs Negative = Positive
  abs x        = fmap abs x

-- Adapted from https://www.schoolofhaskell.com/user/snoyberg/random-code-snippets/storable-instance-of-maybe
instance Storable a => Storable (NegativeInfinite a) where
    sizeOf x = sizeOf (strip x) + 1
    alignment x = alignment (strip x)
    peek ptr = (peekByteOff ptr . sizeOf . strip . stripPtr) ptr >>= \case
      (1 :: Word8) -> NegFinite <$> peek (stripFPtr ptr)
      _ -> pure NegativeInfinity
    poke ptr NegativeInfinity
      = pokeByteOff ptr ((sizeOf . strip . stripPtr) ptr) (0 :: Word8)
    poke ptr (NegFinite a)
      = poke (stripFPtr ptr) a
     *> pokeByteOff ptr (sizeOf a) (1 :: Word8)

instance Storable a => Storable (PositiveInfinite a) where
    sizeOf x = sizeOf (strip x) + 1
    alignment x = alignment (strip x)
    peek ptr = (peekByteOff ptr . sizeOf . strip . stripPtr) ptr >>= \case
      (1 :: Word8) -> PosFinite <$> peek (stripFPtr ptr)
      _ -> pure PositiveInfinity
    poke ptr PositiveInfinity
      = pokeByteOff ptr ((sizeOf . strip . stripPtr) ptr) (0 :: Word8)
    poke ptr (PosFinite a)
      = poke (stripFPtr ptr) a
     *> pokeByteOff ptr (sizeOf a) (1 :: Word8)

instance Storable a => Storable (Infinite a) where
    sizeOf x = sizeOf (strip x) + 1
    alignment x = alignment (strip x)
    peek ptr = (peekByteOff ptr . sizeOf . strip . stripPtr) ptr >>= \case
      0 -> pure Negative
      (1 :: Word8) -> Finite <$> peek (stripFPtr ptr)
      _ -> pure Positive
    poke ptr Positive
      = pokeByteOff ptr ((sizeOf . strip . stripPtr) ptr) (2 :: Word8)
    poke ptr Negative
      = pokeByteOff ptr ((sizeOf . strip . stripPtr) ptr) (0 :: Word8)
    poke ptr (Finite a)
      = poke (stripFPtr ptr) a
     *> pokeByteOff ptr (sizeOf a) (1 :: Word8)

strip :: f a -> a
strip _ = error "strip"

stripFPtr :: Ptr (f a) -> Ptr a
stripFPtr = castPtr

stripPtr :: Ptr a -> a
stripPtr _ = error "stripPtr"

instance NFData a =>
         NFData (NegativeInfinite a) where
    rnf NegativeInfinity = ()
    rnf (NegFinite x)    = rnf x

instance NFData a =>
         NFData (PositiveInfinite a) where
    rnf PositiveInfinity = ()
    rnf (PosFinite x)    = rnf x

instance NFData a =>
         NFData (Infinite a) where
    rnf Negative   = ()
    rnf Positive   = ()
    rnf (Finite x) = rnf x

instance Eq1 NegativeInfinite where
    liftEq eq = go
      where
        go NegativeInfinity NegativeInfinity = True
        go (NegFinite x) (NegFinite y)       = eq x y
        go _ _                               = False

instance Eq1 PositiveInfinite where
    liftEq eq = go
      where
        go PositiveInfinity PositiveInfinity = True
        go (PosFinite x) (PosFinite y)       = eq x y
        go _ _                               = False

instance Eq1 Infinite where
    liftEq eq = go
      where
        go Positive Positive     = True
        go Negative Negative     = True
        go (Finite x) (Finite y) = eq x y
        go _ _                   = False

instance Ord1 NegativeInfinite where
    liftCompare cmp = go
      where
        go NegativeInfinity NegativeInfinity = EQ
        go (NegFinite x) (NegFinite y)       = cmp x y
        go NegativeInfinity (NegFinite _)    = LT
        go (NegFinite _) NegativeInfinity    = GT

instance Ord1 PositiveInfinite where
    liftCompare cmp = go
      where
        go PositiveInfinity PositiveInfinity = EQ
        go (PosFinite x) (PosFinite y)       = cmp x y
        go PositiveInfinity (PosFinite _)    = GT
        go (PosFinite _) PositiveInfinity    = LT

instance Ord1 Infinite where
    liftCompare cmp = go
      where
        go Positive Positive     = EQ
        go Positive Negative     = GT
        go Negative Positive     = LT
        go Negative Negative     = EQ
        go Positive (Finite _)   = GT
        go Negative (Finite _)   = LT
        go (Finite _) Positive   = LT
        go (Finite _) Negative   = GT
        go (Finite x) (Finite y) = cmp x y

instance Show1 PositiveInfinite where
    liftShowsPrec sp _ n = go
      where
        go PositiveInfinity = showString "PositiveInfinity"
        go (PosFinite x) =
            showParen (n > 10) $ showString "PosFinite " . sp 11 x

instance Show1 NegativeInfinite where
    liftShowsPrec sp _ n = go
      where
        go NegativeInfinity = showString "NegativeInfinity"
        go (NegFinite x) =
            showParen (n > 10) $ showString "NegFinite " . sp 11 x

instance Show1 Infinite where
    liftShowsPrec sp _ n = go
      where
        go Positive = showString "Positive"
        go Negative = showString "Negative"
        go (Finite x) =
            showParen (n > 10) $ showString "Finite " . sp 11 x

instance Read1 PositiveInfinite where
    liftReadsPrec rp _ =
        readPrec_to_S $
        parens $
        (do Ident "PositiveInfinity" <- lexP
            pure PositiveInfinity) +++
        prec
            10
            (do Ident "PosFinite" <- lexP
                m <- step (readS_to_Prec rp)
                pure (PosFinite m))

instance Read1 NegativeInfinite where
    liftReadsPrec rp _ =
        readPrec_to_S $
        parens $
        (do Ident "NegativeInfinity" <- lexP
            pure NegativeInfinity) +++
        prec
            10
            (do Ident "NegFinite" <- lexP
                m <- step (readS_to_Prec rp)
                pure (NegFinite m))

instance Read1 Infinite where
    liftReadsPrec rp _ =
        readPrec_to_S $
        parens $
        (do Ident "Negative" <- lexP
            pure Negative) +++
        (do Ident "Positive" <- lexP
            pure Positive) +++
        prec
            10
            (do Ident "Finite" <- lexP
                m <- step (readS_to_Prec rp)
                pure (Finite m))


data instance
     U.MVector s
       (NegativeInfinite
          a) = MV_NegativeInfinite {-# UNPACK #-} !(U.MVector s Bool)
                                   !(U.MVector s a)


data instance
     U.Vector
       (NegativeInfinite a) = V_NegativeInfinite {-# UNPACK #-} !(U.Vector
                                                                    Bool)
                                                 !(U.Vector a)

instance U.Unbox a => U.Unbox (NegativeInfinite a)

instance (U.Unbox a) =>
         M.MVector U.MVector (NegativeInfinite a) where
    {-# INLINE basicLength #-}
    basicLength (MV_NegativeInfinite xs _) = M.basicLength xs
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i_ m_ (MV_NegativeInfinite as bs) =
        MV_NegativeInfinite
            (M.basicUnsafeSlice i_ m_ as)
            (M.basicUnsafeSlice i_ m_ bs)
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_NegativeInfinite as1 bs1) (MV_NegativeInfinite as2 bs2) =
        M.basicOverlaps as1 as2 || M.basicOverlaps bs1 bs2
    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n_ =
        liftA2
            MV_NegativeInfinite
            (M.basicUnsafeNew n_)
            (M.basicUnsafeNew n_)
    {-# INLINE basicInitialize #-}
    basicInitialize (MV_NegativeInfinite as bs) =
        M.basicInitialize as *> M.basicInitialize bs
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n_ NegativeInfinity =
        liftA2
            MV_NegativeInfinite
            (M.basicUnsafeReplicate n_ False)
            (M.basicUnsafeNew n_)
    basicUnsafeReplicate n_ (NegFinite x) =
        liftA2
            MV_NegativeInfinite
            (M.basicUnsafeReplicate n_ True)
            (M.basicUnsafeReplicate n_ x)
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_NegativeInfinite as bs) i_ =
        M.basicUnsafeRead as i_ >>=
        bool (pure NegativeInfinity) (NegFinite <$> M.basicUnsafeRead bs i_)
    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_NegativeInfinite as _) i_ NegativeInfinity =
        M.basicUnsafeWrite as i_ False
    basicUnsafeWrite (MV_NegativeInfinite as bs) i_ (NegFinite x) =
        M.basicUnsafeWrite as i_ True *> M.basicUnsafeWrite bs i_ x
    {-# INLINE basicClear #-}
    basicClear (MV_NegativeInfinite as bs) =
        M.basicClear as *> M.basicClear bs
    {-# INLINE basicSet #-}
    basicSet (MV_NegativeInfinite as bs) NegativeInfinity =
        M.basicSet as False *> M.basicClear bs
    basicSet (MV_NegativeInfinite as bs) (NegFinite x) =
        M.basicSet as True *> M.basicSet bs x
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_NegativeInfinite as1 bs1) (MV_NegativeInfinite as2 bs2) =
        M.basicUnsafeCopy as1 as2 *> M.basicUnsafeCopy bs1 bs2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MV_NegativeInfinite as1 bs1) (MV_NegativeInfinite as2 bs2) =
        M.basicUnsafeMove as1 as2 *> M.basicUnsafeMove bs1 bs2
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_NegativeInfinite as bs) m_ =
        liftA2
            MV_NegativeInfinite
            (M.basicUnsafeGrow as m_)
            (M.basicUnsafeGrow bs m_)

instance (U.Unbox a) =>
         G.Vector U.Vector (NegativeInfinite a) where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_NegativeInfinite as bs) =
        liftA2
            V_NegativeInfinite
            (G.basicUnsafeFreeze as)
            (G.basicUnsafeFreeze bs)
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_NegativeInfinite as bs) =
        liftA2
            MV_NegativeInfinite
            (G.basicUnsafeThaw as)
            (G.basicUnsafeThaw bs)
    {-# INLINE basicLength #-}
    basicLength (V_NegativeInfinite xs _) = G.basicLength xs
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i_ m_ (V_NegativeInfinite as bs) =
        V_NegativeInfinite
            (G.basicUnsafeSlice i_ m_ as)
            (G.basicUnsafeSlice i_ m_ bs)
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_NegativeInfinite as bs) i_ =
        G.basicUnsafeIndexM as i_ >>=
        bool (pure NegativeInfinity) (NegFinite <$> G.basicUnsafeIndexM bs i_)
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_NegativeInfinite as1 bs1) (V_NegativeInfinite as2 bs2) =
        G.basicUnsafeCopy as1 as2 *> G.basicUnsafeCopy bs1 bs2
    {-# INLINE elemseq #-}
    elemseq _ NegativeInfinity b = b
    elemseq _ (NegFinite x) b = G.elemseq (undefined :: U.Vector a) x b

data instance
     U.MVector s
       (PositiveInfinite
          a) = MV_PositiveInfinite {-# UNPACK #-} !(U.MVector s Bool)
                                   !(U.MVector s a)


data instance
     U.Vector
       (PositiveInfinite a) = V_PositiveInfinite {-# UNPACK #-} !(U.Vector
                                                                    Bool)
                                                 !(U.Vector a)

instance U.Unbox a => U.Unbox (PositiveInfinite a)

instance (U.Unbox a) =>
         M.MVector U.MVector (PositiveInfinite a) where
    {-# INLINE basicLength #-}
    basicLength (MV_PositiveInfinite xs _) = M.basicLength xs
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i_ m_ (MV_PositiveInfinite as bs) =
        MV_PositiveInfinite
            (M.basicUnsafeSlice i_ m_ as)
            (M.basicUnsafeSlice i_ m_ bs)
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_PositiveInfinite as1 bs1) (MV_PositiveInfinite as2 bs2) =
        M.basicOverlaps as1 as2 || M.basicOverlaps bs1 bs2
    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n_ =
        liftA2
            MV_PositiveInfinite
            (M.basicUnsafeNew n_)
            (M.basicUnsafeNew n_)
    {-# INLINE basicInitialize #-}
    basicInitialize (MV_PositiveInfinite as bs) =
        M.basicInitialize as *> M.basicInitialize bs
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n_ PositiveInfinity =
        liftA2
            MV_PositiveInfinite
            (M.basicUnsafeReplicate n_ False)
            (M.basicUnsafeNew n_)
    basicUnsafeReplicate n_ (PosFinite x) =
        liftA2
            MV_PositiveInfinite
            (M.basicUnsafeReplicate n_ True)
            (M.basicUnsafeReplicate n_ x)
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_PositiveInfinite as bs) i_ =
        M.basicUnsafeRead as i_ >>=
        bool (pure PositiveInfinity) (PosFinite <$> M.basicUnsafeRead bs i_)
    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_PositiveInfinite as _) i_ PositiveInfinity =
        M.basicUnsafeWrite as i_ False
    basicUnsafeWrite (MV_PositiveInfinite as bs) i_ (PosFinite x) =
        M.basicUnsafeWrite as i_ True *> M.basicUnsafeWrite bs i_ x
    {-# INLINE basicClear #-}
    basicClear (MV_PositiveInfinite as bs) =
        M.basicClear as *> M.basicClear bs
    {-# INLINE basicSet #-}
    basicSet (MV_PositiveInfinite as bs) PositiveInfinity =
        M.basicSet as False *> M.basicClear bs
    basicSet (MV_PositiveInfinite as bs) (PosFinite x) =
        M.basicSet as True *> M.basicSet bs x
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_PositiveInfinite as1 bs1) (MV_PositiveInfinite as2 bs2) =
        M.basicUnsafeCopy as1 as2 *> M.basicUnsafeCopy bs1 bs2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MV_PositiveInfinite as1 bs1) (MV_PositiveInfinite as2 bs2) =
        M.basicUnsafeMove as1 as2 *> M.basicUnsafeMove bs1 bs2
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_PositiveInfinite as bs) m_ =
        liftA2
            MV_PositiveInfinite
            (M.basicUnsafeGrow as m_)
            (M.basicUnsafeGrow bs m_)

instance (U.Unbox a) =>
         G.Vector U.Vector (PositiveInfinite a) where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_PositiveInfinite as bs) =
        liftA2
            V_PositiveInfinite
            (G.basicUnsafeFreeze as)
            (G.basicUnsafeFreeze bs)
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_PositiveInfinite as bs) =
        liftA2
            MV_PositiveInfinite
            (G.basicUnsafeThaw as)
            (G.basicUnsafeThaw bs)
    {-# INLINE basicLength #-}
    basicLength (V_PositiveInfinite xs _) = G.basicLength xs
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i_ m_ (V_PositiveInfinite as bs) =
        V_PositiveInfinite
            (G.basicUnsafeSlice i_ m_ as)
            (G.basicUnsafeSlice i_ m_ bs)
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_PositiveInfinite as bs) i_ =
        G.basicUnsafeIndexM as i_ >>=
        bool (pure PositiveInfinity) (PosFinite <$> G.basicUnsafeIndexM bs i_)
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_PositiveInfinite as1 bs1) (V_PositiveInfinite as2 bs2) =
        G.basicUnsafeCopy as1 as2 *> G.basicUnsafeCopy bs1 bs2
    {-# INLINE elemseq #-}
    elemseq _ PositiveInfinity b = b
    elemseq _ (PosFinite x) b = G.elemseq (undefined :: U.Vector a) x b

data instance
     U.MVector s (Infinite a) = MV_Infinite {-# UNPACK #-} !(U.MVector s
                                                               Word8)
                                            !(U.MVector s a)


data instance
     U.Vector (Infinite a) = V_Infinite {-# UNPACK #-} !(U.Vector Word8)
                                        !(U.Vector a)

instance U.Unbox a => U.Unbox (Infinite a)

instance (U.Unbox a) =>
         M.MVector U.MVector (Infinite a) where
    {-# INLINE basicLength #-}
    basicLength (MV_Infinite xs _) = M.basicLength xs
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i_ m_ (MV_Infinite as bs) =
        MV_Infinite
            (M.basicUnsafeSlice i_ m_ as)
            (M.basicUnsafeSlice i_ m_ bs)
    {-# INLINE basicOverlaps #-}
    basicOverlaps (MV_Infinite as1 bs1) (MV_Infinite as2 bs2) =
        M.basicOverlaps as1 as2 || M.basicOverlaps bs1 bs2
    {-# INLINE basicUnsafeNew #-}
    basicUnsafeNew n_ =
        liftA2
            MV_Infinite
            (M.basicUnsafeNew n_)
            (M.basicUnsafeNew n_)
    {-# INLINE basicInitialize #-}
    basicInitialize (MV_Infinite as bs) =
        M.basicInitialize as *> M.basicInitialize bs
    {-# INLINE basicUnsafeReplicate #-}
    basicUnsafeReplicate n_ Positive =
        liftA2
            MV_Infinite
            (M.basicUnsafeReplicate n_ 2)
            (M.basicUnsafeNew n_)
    basicUnsafeReplicate n_ Negative =
        liftA2
            MV_Infinite
            (M.basicUnsafeReplicate n_ 0)
            (M.basicUnsafeNew n_)
    basicUnsafeReplicate n_ (Finite x) =
        liftA2
            MV_Infinite
            (M.basicUnsafeReplicate n_ 1)
            (M.basicUnsafeReplicate n_ x)
    {-# INLINE basicUnsafeRead #-}
    basicUnsafeRead (MV_Infinite as bs) i_ =
        M.basicUnsafeRead as i_ >>= \case
          0 -> pure Negative
          1 -> Finite <$> M.basicUnsafeRead bs i_
          _ -> pure Positive
    {-# INLINE basicUnsafeWrite #-}
    basicUnsafeWrite (MV_Infinite as _) i_ Positive =
        M.basicUnsafeWrite as i_ 2
    basicUnsafeWrite (MV_Infinite as _) i_ Negative =
        M.basicUnsafeWrite as i_ 0
    basicUnsafeWrite (MV_Infinite as bs) i_ (Finite x) =
        M.basicUnsafeWrite as i_ 1 *> M.basicUnsafeWrite bs i_ x
    {-# INLINE basicClear #-}
    basicClear (MV_Infinite as bs) =
        M.basicClear as *> M.basicClear bs
    {-# INLINE basicSet #-}
    basicSet (MV_Infinite as bs) Positive =
        M.basicSet as 2 *> M.basicClear bs
    basicSet (MV_Infinite as bs) Negative =
        M.basicSet as 0 *> M.basicClear bs
    basicSet (MV_Infinite as bs) (Finite x) =
        M.basicSet as 1 *> M.basicSet bs x
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_Infinite as1 bs1) (MV_Infinite as2 bs2) =
        M.basicUnsafeCopy as1 as2 *> M.basicUnsafeCopy bs1 bs2
    {-# INLINE basicUnsafeMove #-}
    basicUnsafeMove (MV_Infinite as1 bs1) (MV_Infinite as2 bs2) =
        M.basicUnsafeMove as1 as2 *> M.basicUnsafeMove bs1 bs2
    {-# INLINE basicUnsafeGrow #-}
    basicUnsafeGrow (MV_Infinite as bs) m_ =
        liftA2
            MV_Infinite
            (M.basicUnsafeGrow as m_)
            (M.basicUnsafeGrow bs m_)

instance (U.Unbox a) =>
         G.Vector U.Vector (Infinite a) where
    {-# INLINE basicUnsafeFreeze #-}
    basicUnsafeFreeze (MV_Infinite as bs) =
        liftA2
            V_Infinite
            (G.basicUnsafeFreeze as)
            (G.basicUnsafeFreeze bs)
    {-# INLINE basicUnsafeThaw #-}
    basicUnsafeThaw (V_Infinite as bs) =
        liftA2
            MV_Infinite
            (G.basicUnsafeThaw as)
            (G.basicUnsafeThaw bs)
    {-# INLINE basicLength #-}
    basicLength (V_Infinite xs _) = G.basicLength xs
    {-# INLINE basicUnsafeSlice #-}
    basicUnsafeSlice i_ m_ (V_Infinite as bs) =
        V_Infinite
            (G.basicUnsafeSlice i_ m_ as)
            (G.basicUnsafeSlice i_ m_ bs)
    {-# INLINE basicUnsafeIndexM #-}
    basicUnsafeIndexM (V_Infinite as bs) i_ =
        G.basicUnsafeIndexM as i_ >>= \case
          0 -> pure Negative
          1 -> Finite <$> G.basicUnsafeIndexM bs i_
          _ -> pure Positive
    {-# INLINE basicUnsafeCopy #-}
    basicUnsafeCopy (MV_Infinite as1 bs1) (V_Infinite as2 bs2) =
        G.basicUnsafeCopy as1 as2 *> G.basicUnsafeCopy bs1 bs2
    {-# INLINE elemseq #-}
    elemseq _ Positive b = b
    elemseq _ Negative b = b
    elemseq _ (Finite x) b = G.elemseq (undefined :: U.Vector a) x b
