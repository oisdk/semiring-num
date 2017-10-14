{-# LANGUAGE DeriveFoldable        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Vectors where

import           Test.QuickCheck
import           Test.SmallCheck.Series

import           Data.Functor.Classes

import           CompUtils
import           Control.Applicative
import           Data.Foldable

data V0 a =
    V0
    deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data V1 a =
    V1 a
    deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data V2 a =
    V2 a a
    deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data V3 a =
    V3 a a a
    deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data V4 a =
    V4 a a a a
    deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

data V5 a =
    V5 a a a a a
    deriving (Eq,Ord,Show,Functor,Foldable,Traversable)

instance Applicative V0 where
    pure _ = V0
    V0 <*> V0 = V0

instance Applicative V1 where
    pure = V1
    V1 f <*> V1 x = V1 (f x)

instance Applicative V2 where
    pure x = V2 x x
    V2 f1 f2 <*> V2 x1 x2 = V2 (f1 x1) (f2 x2)

instance Applicative V3 where
    pure x = V3 x x x
    V3 f1 f2 f3 <*> V3 x1 x2 x3 = V3 (f1 x1) (f2 x2) (f3 x3)

instance Applicative V4 where
    pure x = V4 x x x x
    V4 f1 f2 f3 f4 <*> V4 x1 x2 x3 x4 = V4 (f1 x1) (f2 x2) (f3 x3) (f4 x4)

instance Applicative V5 where
    pure x = V5 x x x x x
    V5 f1 f2 f3 f4 f5 <*> V5 x1 x2 x3 x4 x5 =
        V5 (f1 x1) (f2 x2) (f3 x3) (f4 x4) (f5 x5)

instance Arbitrary a =>
         Arbitrary (V0 a) where
    arbitrary = sequenceA (pure arbitrary)
    shrink V0 = []

instance Arbitrary a =>
         Arbitrary (V1 a) where
    arbitrary = sequenceA (pure arbitrary)
    shrink (V1 x) = map V1 (shrink x)

instance Arbitrary a =>
         Arbitrary (V2 a) where
    arbitrary = sequenceA (pure arbitrary)
    shrink (V2 x y) =
        [ V2 x' y
        | x' <- shrink x ] ++
        [ V2 x y'
        | y' <- shrink y ]

instance Arbitrary a =>
         Arbitrary (V3 a) where
    arbitrary = sequenceA (pure arbitrary)
    shrink (V3 x y z) =
        [ V3 x' y z
        | x' <- shrink x ] ++
        [ V3 x y' z
        | y' <- shrink y ] ++
        [ V3 x y z'
        | z' <- shrink z ]

instance Arbitrary a =>
         Arbitrary (V4 a) where
    arbitrary = sequenceA (pure arbitrary)
    shrink (V4 w x y z) =
        [ V4 w' x y z
        | w' <- shrink w ] ++
        [ V4 w x' y z
        | x' <- shrink x ] ++
        [ V4 w x y' z
        | y' <- shrink y ] ++
        [ V4 w x y z'
        | z' <- shrink z ]

instance Arbitrary a =>
         Arbitrary (V5 a) where
    arbitrary = sequenceA (pure arbitrary)
    shrink (V5 v w x y z) =
        [ V5 v' w x y z
        | v' <- shrink v ] ++
        [ V5 v w' x y z
        | w' <- shrink w ] ++
        [ V5 v w x' y z
        | x' <- shrink x ] ++
        [ V5 v w x y' z
        | y' <- shrink y ] ++
        [ V5 v w x y z'
        | z' <- shrink z ]

instance Serial m a => Serial m (V0 a) where
    series = cons0 V0

instance Serial m a => Serial m (V1 a) where
    series = cons1 V1

instance Serial m a => Serial m (V2 a) where
    series = cons2 V2

instance Serial m a => Serial m (V3 a) where
    series = cons3 V3

instance Serial m a => Serial m (V4 a) where
    series = cons4 V4

instance Serial m a =>
         Serial m (V5 a) where
    series =
        decDepth $ V5 <$> series <~> series <~> series <~> series <~> series

instance Eq1 V0 where
    liftEq eq = and .: liftA2 eq

instance Eq1 V1 where
    liftEq eq = and .: liftA2 eq

instance Eq1 V2 where
    liftEq eq = and .: liftA2 eq

instance Eq1 V3 where
    liftEq eq = and .: liftA2 eq

instance Eq1 V4 where
    liftEq eq = and .: liftA2 eq

instance Eq1 V5 where
    liftEq eq = and .: liftA2 eq

instance Ord1 V0 where
    liftCompare cmp = fold .: liftA2 cmp

instance Ord1 V1 where
    liftCompare cmp = fold .: liftA2 cmp

instance Ord1 V2 where
    liftCompare cmp = fold .: liftA2 cmp

instance Ord1 V3 where
    liftCompare cmp = fold .: liftA2 cmp

instance Ord1 V4 where
    liftCompare cmp = fold .: liftA2 cmp

instance Ord1 V5 where
    liftCompare cmp = fold .: liftA2 cmp

instance Show1 V0 where
    liftShowsPrec _ sl _ = sl . toList

instance Show1 V1 where
    liftShowsPrec _ sl _ = sl . toList

instance Show1 V2 where
    liftShowsPrec _ sl _ = sl . toList

instance Show1 V3 where
    liftShowsPrec _ sl _ = sl . toList

instance Show1 V4 where
    liftShowsPrec _ sl _ = sl . toList

instance Show1 V5 where
    liftShowsPrec _ sl _ = sl . toList
