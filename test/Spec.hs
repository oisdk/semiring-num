module Main (main) where

import           Control.Applicative   (liftA2)
import           Data.Foldable
import qualified Data.Map.Strict       as Map
import           Data.Monoid
import           Data.Semiring
import           Data.Semiring.Free
import           Data.Semiring.Numeric
import           Data.Set              (Set)
import           Data.Word             (Word8)
import           Test.DocTest
import           Test.QuickCheck
import           Test.Semiring

smallCheck :: Testable prop => prop -> IO ()
smallCheck = quickCheckWith (stdArgs { maxSuccess = 40, maxSize = 30})

instance (Enum a, Bounded a, Ord a, Ord b, Semiring b) => Semiring (Func a b) where
  zero = fromFunc zero
  one = fromFunc one
  f <+> g = fromFunc (apply f <+> apply g)
  f <.> g = fromFunc (apply f <.> apply g)


main :: IO ()
main = do
  quickCheck (semiringLaws :: Laws (Func Word8 Word8))
  quickCheck (semiringLaws :: Laws ())
  quickCheck (semiringLaws :: Laws Bool)
  quickCheck (forAll arbitrary (\(x,y,z) -> semiringLaws (Any x) (Any y) (Any z)))
  quickCheck (forAll arbitrary (\(x,y,z) -> semiringLaws (All x) (All y) (All z)))
  quickCheck (semiringLaws :: Laws ())
  quickCheck (semiringLaws :: Laws Integer)
  smallCheck (semiringLaws :: Laws (Set (Add Integer)))
  smallCheck (semiringLaws :: Laws [Integer])
  quickCheck (semiringLaws :: Laws (Max Integer))
  quickCheck (semiringLaws :: Laws (Min Integer))
  quickCheck (semiringLaws :: Laws (Integer,Integer))
  quickCheck (semiringLaws :: Laws (Integer,Integer,Integer))
  quickCheck (semiringLaws :: Laws (Integer,Integer,Integer,Integer))
  quickCheck (semiringLaws :: Laws (Integer,Integer,Integer,Integer,Integer))
  smallCheck (semiringLaws :: Laws (Free Integer))
  quickCheck (semiringLaws :: Laws (Bottleneck Word8))
  quickCheck (semiringLaws :: Laws (Division Integer))
  quickCheck (semiringLaws :: Laws (Łukasiewicz Integer))
  quickCheck (semiringLaws :: Laws (Viterbi Integer))
  doctest [ "-isrc"
          , "src/Data/Semiring.hs"
          , "src/Data/Semiring/Numeric.hs"
          , "src/Test/Semiring.hs"
          , "src/Data/Semiring/Free.hs" ]

data Func a b = Func b [(a,b)]
  deriving (Eq, Ord)

instance (Show a, Show b) => Show (Func a b) where
  showsPrec _ (Func c xs)  = showChar '{' . foldr f b xs where
    f (x,y) a = shows x . showString " -> " . shows y . showString ", " . a
    b = showString "_ -> " . shows c . showChar '}'

apply :: Ord a => Func a b -> a -> b
apply (Func c cs) x = foldr f c cs where
  f (e,y) a = case compare x e of
    LT -> c
    EQ -> y
    GT -> a

instance (Ord a, Eq b, Arbitrary a, Arbitrary b) => Arbitrary (Func a b) where
  arbitrary = liftA2 fromList arbitrary arbitrary
  shrink (Func c xs) = map (fromList c) (shrink xs)

fromList :: (Ord a, Eq b) => b -> [(a,b)] -> Func a b
fromList cnst
  = Func cnst
  . Map.toList
  . Map.fromList
  . filter ((cnst/=) . snd)

fromFunc :: (Enum a, Bounded a, Ord a, Ord b) => (a -> b) -> Func a b
fromFunc f = fromList cnst (zip xs ys) where
  xs = [minBound..maxBound]
  ys = map f xs
  Just cnst = mostFrequent ys

mostFrequent :: (Ord a, Foldable f) => f a -> Maybe a
mostFrequent = fmap fst . fst . foldl' f (Nothing, Map.empty :: Map.Map a Int) where
  f (b,m) e = (Just nb, Map.insert e c m) where
    c = maybe 1 succ (Map.lookup e m)
    nb = case b of
      Just (a,d) | d >= c -> (a,d)
      _ -> (e,c)
