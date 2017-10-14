{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module LimitSize where

import           Test.QuickCheck
import           Test.SmallCheck.Series

import           Data.Semiring

import           GHC.TypeLits
import           Data.Function
import           Data.Proxy

newtype LimitSize (n :: Nat) a =
    LimitSize [a]
    deriving (Arbitrary,Semiring,DetectableZero,StarSemiring)

takeFirst
    :: KnownNat n
    => LimitSize n a -> [a]
takeFirst (LimitSize xs :: LimitSize n a) =
    take (fromInteger (natVal (Proxy :: Proxy n))) xs

instance Serial m a =>
         Serial m (LimitSize n a) where
    series = fmap LimitSize series

instance (Eq a, KnownNat n) =>
         Eq (LimitSize n a) where
    (==) = (==) `on` takeFirst

instance (Show a, KnownNat n) =>
         Show (LimitSize n a) where
    showsPrec n = showsPrec n . takeFirst
