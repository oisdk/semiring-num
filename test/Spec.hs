{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main (main) where

import           Control.Applicative
import           Control.Arrow          (first)
import           Data.Bits              ((.&.))
import           Data.Foldable
import           Data.Function          (on)
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.Map.Strict        as Map
import           Data.Monoid
import           Data.Semigroup         (Max (..), Min (..))
import           Data.Semiring
import           Data.Semiring.Free
import           Data.Semiring.Numeric
import qualified Data.Set               as Set
import           Data.Word              (Word8)
import           Test.DocTest
import           Test.Semiring
import           Test.SmallCheck
import           Test.SmallCheck.Series

------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Integer"
  smallCheck 1000 (unaryLaws   :: UnaryLaws   Integer)
  smallCheck 100  (binaryLaws  :: BinaryLaws  Integer)
  smallCheck 10   (ternaryLaws :: TernaryLaws Integer)

  putStrLn "Word2"
  smallCheck 16  (unaryLaws   :: UnaryLaws   Word2)
  smallCheck 16  (binaryLaws  :: BinaryLaws  Word2)
  smallCheck 16  (ternaryLaws :: TernaryLaws Word2)

  putStrLn "(Word2,Word2)"
  smallCheck 16 (unaryLaws   :: UnaryLaws   (Word2,Word2))
  smallCheck 14 (binaryLaws  :: BinaryLaws  (Word2,Word2))
  smallCheck 8  (ternaryLaws :: TernaryLaws (Word2,Word2))

  putStrLn "(Word2,Word2,Word2)"
  smallCheck 10 (unaryLaws   :: UnaryLaws   (Word2,Word2,Word2))
  smallCheck 5  (binaryLaws  :: BinaryLaws  (Word2,Word2,Word2))
  smallCheck 2  (ternaryLaws :: TernaryLaws (Word2,Word2,Word2))

  putStrLn "(Word2,Word2,Word2,Word2)"
  smallCheck 8 (unaryLaws   :: UnaryLaws   (Word2,Word2,Word2,Word2))
  smallCheck 4 (binaryLaws  :: BinaryLaws  (Word2,Word2,Word2,Word2))
  smallCheck 1 (ternaryLaws :: TernaryLaws (Word2,Word2,Word2,Word2))

  putStrLn "Int"
  smallCheck 1000 (unaryLaws   :: UnaryLaws   Int)
  smallCheck 100  (binaryLaws  :: BinaryLaws  Int)
  smallCheck 10   (ternaryLaws :: TernaryLaws Int)

  putStrLn "()"
  smallCheck 1 (unaryLaws   :: UnaryLaws   ())
  smallCheck 1 (binaryLaws  :: BinaryLaws  ())
  smallCheck 1 (ternaryLaws :: TernaryLaws ())

  putStrLn "Bool"
  smallCheck 2 (unaryLaws   :: UnaryLaws   Bool)
  smallCheck 4 (binaryLaws  :: BinaryLaws  Bool)
  smallCheck 8 (ternaryLaws :: TernaryLaws Bool)

  putStrLn "Any"
  smallCheck 2 (unLawsOn   Any :: UnaryLaws   Bool)
  smallCheck 4 (binLawsOn  Any :: BinaryLaws  Bool)
  smallCheck 8 (ternLawsOn Any :: TernaryLaws Bool)

  putStrLn "All"
  smallCheck 2 (unLawsOn   All :: UnaryLaws   Bool)
  smallCheck 4 (binLawsOn  All :: BinaryLaws  Bool)
  smallCheck 8 (ternLawsOn All :: TernaryLaws Bool)

  putStrLn "[Word2]"
  smallCheck 5 (unaryLaws   :: UnaryLaws   [Word2])
  smallCheck 4 (binaryLaws  :: BinaryLaws  [Word2])
  smallCheck 3 (ternaryLaws :: TernaryLaws [Word2])

  putStrLn "Set [Word2]"
  smallCheck 4 (unLawsOn   Set.fromList :: UnaryLaws   [[Word2]])
  smallCheck 3 (binLawsOn  Set.fromList :: BinaryLaws  [[Word2]])
  smallCheck 3 (ternLawsOn Set.fromList :: TernaryLaws [[Word2]])

  putStrLn "Min Integer"
  smallCheck 1000 (unLawsOn   Min :: UnaryLaws   Word2)
  smallCheck 100  (binLawsOn  Min :: BinaryLaws  Word2)
  smallCheck 10   (ternLawsOn Min :: TernaryLaws Word2)

  putStrLn "Max Integer"
  smallCheck 1000 (unLawsOn   Max :: UnaryLaws   Word2)
  smallCheck 100  (binLawsOn  Max :: BinaryLaws  Word2)
  smallCheck 10   (ternLawsOn Max :: TernaryLaws Word2)

  putStrLn "Free Word2"
  smallCheck 4 (unLawsOn   Free :: UnaryLaws   [[Word2]])
  smallCheck 3 (binLawsOn  Free :: BinaryLaws  [[Word2]])
  smallCheck 3 (ternLawsOn Free :: TernaryLaws [[Word2]])

  putStrLn "Bottleneck Word2"
  smallCheck 1000 (unLawsOn   Bottleneck :: UnaryLaws   Word2)
  smallCheck 100  (binLawsOn  Bottleneck :: BinaryLaws  Word2)
  smallCheck 10   (ternLawsOn Bottleneck :: TernaryLaws Word2)

  putStrLn "Division Integer"
  smallCheck 1000 (unLawsOn   (Division . getPositive) :: UnaryLaws   (Positive Integer))
  smallCheck 100  (binLawsOn  (Division . getPositive) :: BinaryLaws  (Positive Integer))
  smallCheck 10   (ternLawsOn (Division . getPositive) :: TernaryLaws (Positive Integer))

  putStrLn "Łukasiewicz Double"
  smallCheck 1000 (unLawsOn   (Łukasiewicz . getFrac) :: UnaryLaws   Fraction)
  smallCheck 100  (binLawsOn  (Łukasiewicz . getFrac) :: BinaryLaws  Fraction)
  smallCheck 10   (ternLawsOn (Łukasiewicz . getFrac) :: TernaryLaws Fraction)

  putStrLn "Viterbi Double"
  smallCheck 1000 (unLawsOn   (Viterbi . getFrac) :: UnaryLaws   Fraction)
  smallCheck 100  (binLawsOn  (Viterbi . getFrac) :: BinaryLaws  Fraction)
  smallCheck 10   (ternLawsOn (Viterbi . getFrac) :: TernaryLaws Fraction)

  putStrLn "Bool -> Bool"
  smallCheck 3 (unLawsOn   fromFunc :: UnaryLaws   (Bool -> Bool))
  smallCheck 2 (binLawsOn  fromFunc :: BinaryLaws  (Bool -> Bool))
  smallCheck 2 (ternLawsOn fromFunc :: TernaryLaws (Bool -> Bool))

  doctest [ "-isrc"
          , "src/Data/Semiring.hs"
          , "src/Data/Semiring/Numeric.hs"
          , "src/Test/Semiring.hs"
          , "src/Data/Semiring/Free.hs" ]

------------------------------------------------------------------------
-- Test helpers

type UnaryLaws   a =           a -> Either String String
type BinaryLaws  a =      a -> a -> Either String String
type TernaryLaws a = a -> a -> a -> Either String String

unLawsOn :: (Eq b, Semiring b, Show b) => (a -> b) -> UnaryLaws a
unLawsOn f = unaryLaws . f

binLawsOn :: (Eq b, Semiring b, Show b) => (a -> b) -> BinaryLaws a
binLawsOn f = binaryLaws `on` f

ternLawsOn :: (Eq b, Semiring b, Show b) => (a -> b) -> TernaryLaws a
ternLawsOn f x y z = ternaryLaws (f x) (f y) (f z)

------------------------------------------------------------------------
-- Serial wrappers

-- | A type with a serial instance between zero and one
newtype Fraction = Fraction { getFrac :: Double } deriving (Show, Eq, Ord)

instance Monad m => Serial m Fraction where
  series = fmap Fraction $ generate (\d -> if d >= 0 then pure 0 else empty) <|> rest where
    rest = generate $ \d -> take d (1 : go 0 1)
    go lower upper = let mid = (lower + upper) / 2 in
      mid : interleave (go lower mid) (go mid upper)
    interleave (x:xs) (y:ys) = x : y : interleave xs ys
    interleave _ _ = undefined

-- | A very small numeric type for exhaustivity
newtype Word2 = Word2 { getWord2 :: Word8 } deriving (Eq, Ord)
instance Show Word2 where show = show . getWord2

instance Bounded Word2 where
  minBound = Word2 0
  maxBound = Word2 3

instance Enum Word2 where
  fromEnum = fromEnum . getWord2
  toEnum x = Word2 (toEnum x .&. maxBound)

instance Num Word2 where
  Word2 x + Word2 y = Word2 ((x + y)   .&. maxBound)
  Word2 x * Word2 y = Word2 ((x * y)   .&. maxBound)
  Word2 x - Word2 y = Word2 ((x - y)   .&. maxBound)
  fromInteger x = Word2 (fromInteger x .&. maxBound)
  abs = id
  signum (Word2 x) = Word2 (signum x)

instance Real Word2 where
  toRational = toRational . getWord2

instance Integral Word2 where
  toInteger = toInteger . getWord2
  quotRem (Word2 x) (Word2 y) = (Word2 (quot x y), Word2 (rem x y))

instance Monad m => Serial m Word2 where
  series = generate (`take` [minBound..maxBound])

instance Semiring Word2

------------------------------------------------------------------------
-- Function Equality

-- | A representation of a function
data Func a b = Func b (IntMap b)
  deriving (Eq, Ord)

fromList' :: Eq b => b -> [(Int,b)] -> Func a b
fromList' cnst
  = Func cnst
  . IntMap.fromList
  . filter ((cnst/=) . snd)

fromList :: (Enum a, Eq b) => b -> [(a,b)] -> Func a b
fromList cnst
  = fromList' cnst
  . map (first fromEnum)

fromFunc :: (Enum a, Bounded a, Ord b) => (a -> b) -> Func a b
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

apply :: Enum a => Func a b -> a -> b
apply (Func c cs) x = IntMap.findWithDefault c (fromEnum x) cs

instance (Enum a, Show a, Show b) => Show (Func a b) where
  showsPrec _ (Func c xs :: Func a b)  = showChar '{' . IntMap.foldrWithKey f b xs where
    f x y a = shows (toEnum x :: a) . showString " -> " . shows y . showString ", " . a
    b = showString "_ -> " . shows c . showChar '}'

instance (Enum a, Bounded a, Ord b, Semiring b) => Semiring (Func a b) where
  zero = fromFunc zero
  one = fromFunc one
  f <+> g = fromFunc (apply f <+> apply g)
  f <.> g = fromFunc (apply f <.> apply g)
