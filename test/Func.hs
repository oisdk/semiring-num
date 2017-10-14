{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Func where

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map

import           Data.Semiring

import           Test.QuickCheck

import           Data.Function
import           Data.Monoid

import           CompUtils

data Func a b = Func
    { def  :: b
    , vals :: IntMap b
    } deriving (Eq,Ord)

(#$) :: Enum a => Func a b -> a -> b
(#$) f x = IntMap.findWithDefault (def f) (fromEnum x) (vals f)

instance (Enum a, Show a, Show b) =>
         Show (Func a b) where
    showsPrec _ (Func c xs :: Func a b) =
        showChar '{' . IntMap.foldrWithKey f b xs
      where
        f x y a =
            shows (toEnum x :: a) .
            showString " -> " . shows y . showString ", " . a
        b = showString "_ -> " . shows c . showChar '}'

fromFunc :: (Enum a, Bounded a, Ord b) => (a -> b) -> Func a b
fromFunc f =
    uncurry Func . fmap IntMap.fromList . remMostFreq $
    [ (fromEnum x, f x)
    | x <- [minBound .. maxBound] ]
  where
    remMostFreq xs = (mf, filter ((mf/=) . snd) xs) where
      Just mf = mostFrequent (map snd xs)

mostFrequent :: Ord a => [a] -> Maybe a
mostFrequent xs = foldr f (const . fmap fst) xs Nothing (Map.empty :: Map a Int) where
  f e a Nothing _ = a (Just (e, 1)) (Map.singleton e 1)
  f e a (Just (b,n)) m
    | d > n = a (Just (e,d)) nm
    | otherwise = a (Just (b,n)) nm where
    (nv,nm) = Map.insertLookupWithKey (const (+)) e 1 m
    d = maybe 1 succ nv

instance (Enum a, Bounded a, Ord b, Semiring b) => Semiring (Func a b) where
    zero = fromFunc zero
    one = fromFunc one
    (<+>) = fromFunc .: (<+>) `on` (#$)
    (<.>) = fromFunc .: (<.>) `on` (#$)

instance (Bounded a, Enum a, Ord b, Arbitrary b, CoArbitrary a) =>
         Arbitrary (Func a b) where
    arbitrary = fmap fromFunc arbitrary
    shrink f = fmap fromFunc (shrink (f #$))

newtype EndoFunc a = EndoFunc
    { getEndoFunc :: Endo a
    } deriving (Semiring,DetectableZero)

instance (Enum a, Bounded a, Ord a) => Eq (EndoFunc a) where
  (==) = (==) `on` (fromFunc .# appEndo .# getEndoFunc)

instance (Enum a, Bounded a, Ord a, Show a) => Show (EndoFunc a) where
  showsPrec n = showsPrec n . fromFunc .# appEndo .# getEndoFunc

instance (Arbitrary a, CoArbitrary a) =>
         Arbitrary (EndoFunc a) where
    arbitrary = (EndoFunc . Endo) <#$> arbitrary
    shrink (EndoFunc (Endo f)) = (EndoFunc . Endo) <#$> shrink f
