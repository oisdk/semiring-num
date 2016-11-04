{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The Free semiring.
module Data.Semiring.Free
  ( Free(..)
  , liftFree
  , unFree
  ) where

import           Control.Applicative (liftA2)

import           Data.Coerce

import           Data.Function (on)
import           Data.List (sort)

import           Data.Semiring
import           Data.Ord (comparing)

import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)

-- | The free semiring. Adapted from PureScript's version, available
-- <https://pursuit.purescript.org/packages/purescript-semirings/3.0.0/docs/Data.Semiring.Free here>.
-- Only a valid semiring if treated as a multiset, as in:
--
-- >>> Free [[1],[0]] == Free [[0],[1]]
-- True
newtype Free a = Free
  { getFree :: [[a]]
  } deriving (Show, Read, Functor, Foldable, Traversable, Monoid)

instance Semiring (Free a) where
  Free xs <+> Free ys = Free (xs ++ ys)
  Free xs <.> Free ys = Free (liftA2 (++) xs ys)
  one = Free [[]]
  zero = Free []

instance Applicative Free where
  pure = Free . pure . pure
  Free fs <*> Free xs = Free (liftA2 (<*>) fs xs)

-- | Run a 'Free'.
liftFree :: Semiring s => (a -> s) -> Free a -> s
liftFree f = unFree . fmap f

-- | Run a 'Free', interpreting it in the underlying semiring.
unFree :: Semiring s => Free s -> s
unFree = getAdd .# foldMap (Add .# getMul .# foldMap Mul) . getFree

-- | Extremely slow. For testing purposes.
instance Ord a => Eq (Free a) where
  (==) = isAnagram `on` getFree

instance Ord a => Ord (Free a) where
  compare = comparing (sort . getFree)

infixr 9 .#
(.#) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(.#) _ = coerce

isAnagram :: Ord a => [a] -> [a] -> Bool
isAnagram = go (Map.empty :: Map a Int) where
  go !m (x:xs) (y:ys) =
    go ( Map.alter (remZero . maybe (-1) pred) x
       $ Map.alter (remZero . maybe 1    succ) y
    m) xs ys
  go !m [] [] = Map.null m
  go _ _ _ = False
  remZero 0 = Nothing
  remZero n = Just n
