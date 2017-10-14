{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Fraction where

import           Data.Semiring

import           Test.SmallCheck.Series

import           Control.Applicative

newtype Fraction =
    Fraction Double
    deriving (Show,Num,Fractional,Real,RealFrac,Floating,RealFloat,Semiring)

instance DetectableZero Fraction where isZero = (0==)

instance Eq Fraction where
    Fraction x == Fraction y = abs (x - y) < 0.011

instance Ord Fraction where
    compare (Fraction x) (Fraction y)
      | Fraction x == Fraction y = EQ
      | otherwise = compare x y

instance Monad m => Serial m Fraction where
  series = fmap Fraction $ generate (\d -> if d >= 0 then pure 0 else empty) <|> rest where
    rest = generate $ \d -> take d (1 : go 0 1)
    go lower upper = let mid = (lower + upper) / 2 in
      mid : interleave (go lower mid) (go mid upper)
    interleave (x:xs) (y:ys) = x : y : interleave xs ys
    interleave _ _           = undefined
