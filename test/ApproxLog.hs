{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module ApproxLog where

import           Test.QuickCheck as QC
import           Test.SmallCheck.Series as SC

import           Data.Semiring

import           Numeric.Log

import           CompUtils

newtype ApproxLog a =
    ApproxLog (Log a)
    deriving (Show,Num,Fractional,Real,RealFrac,Floating,Semiring)

instance (Arbitrary a, Precise a, RealFloat a) =>
         Arbitrary (ApproxLog a) where
    arbitrary = fmap (ApproxLog #. fromRational .# QC.getNonNegative) arbitrary

instance (Serial m a, Precise a, RealFloat a) =>
         Serial m (ApproxLog a) where
    series = fmap (ApproxLog #. fromRational .# SC.getNonNegative) series

instance (RealFloat a, Ord a) =>
         Eq (ApproxLog a) where
    ApproxLog (Exp x) == ApproxLog (Exp y) =
        isInfinite x && isInfinite y ||
        x == y || abs ((exp x-exp y) / exp x) < 0.01

instance (RealFloat a, Ord a) => Ord (ApproxLog a) where
    compare (ApproxLog x) (ApproxLog y)
      | ApproxLog x == ApproxLog y = EQ
      | otherwise = compare x y
