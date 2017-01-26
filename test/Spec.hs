{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE KindSignatures             #-}

module Main (main) where

import           Control.Applicative
import           Control.Arrow          (first)
import           Data.Foldable
import           Data.IntMap.Strict     (IntMap)
import qualified Data.IntMap.Strict     as IntMap
import qualified Data.Map.Strict        as Map
import           Data.Monoid
import           Data.Semiring
import           Data.Semiring.Free
import           Data.Semiring.Numeric
import qualified Data.Set               as Set
import           Test.DocTest
import           Test.Semiring
import           Test.SmallCheck
import           Test.SmallCheck.Series
import           GHC.TypeLits
import           Data.Function
import           Data.Bits

------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Integer"
  smallCheck 1000 (unaryLaws   :: UnaryLaws   Integer)
  smallCheck 100  (binaryLaws  :: BinaryLaws  Integer)
  smallCheck 10   (ternaryLaws :: TernaryLaws Integer)

  putStrLn "(WordN 2)"
  smallCheck 16  (unaryLaws   :: UnaryLaws   (WordN 2))
  smallCheck 16  (binaryLaws  :: BinaryLaws  (WordN 2))
  smallCheck 16  (ternaryLaws :: TernaryLaws (WordN 2))

  putStrLn "(WordN 2,WordN 2)"
  smallCheck 16 (unaryLaws   :: UnaryLaws   (WordN 2,WordN 2))
  smallCheck 14 (binaryLaws  :: BinaryLaws  (WordN 2,WordN 2))
  smallCheck 8  (ternaryLaws :: TernaryLaws (WordN 2,WordN 2))

  putStrLn "(WordN 2,WordN 2,WordN 2)"
  smallCheck 10 (unaryLaws   :: UnaryLaws   (WordN 2,WordN 2,WordN 2))
  smallCheck 5  (binaryLaws  :: BinaryLaws  (WordN 2,WordN 2,WordN 2))
  smallCheck 2  (ternaryLaws :: TernaryLaws (WordN 2,WordN 2,WordN 2))

  putStrLn "(WordN 2,WordN 2,WordN 2,WordN 2)"
  smallCheck 8 (unaryLaws   :: UnaryLaws   (WordN 2,WordN 2,WordN 2,WordN 2))
  smallCheck 4 (binaryLaws  :: BinaryLaws  (WordN 2,WordN 2,WordN 2,WordN 2))
  smallCheck 1 (ternaryLaws :: TernaryLaws (WordN 2,WordN 2,WordN 2,WordN 2))

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

  putStrLn "[WordN 2]"
  smallCheck 5 (unaryLaws   :: UnaryLaws   [WordN 2])
  smallCheck 4 (binaryLaws  :: BinaryLaws  [WordN 2])
  smallCheck 3 (ternaryLaws :: TernaryLaws [WordN 2])

  putStrLn "Set [WordN 2]"
  smallCheck 4 (unLawsOn   Set.fromList :: UnaryLaws   [[WordN 2]])
  smallCheck 3 (binLawsOn  Set.fromList :: BinaryLaws  [[WordN 2]])
  smallCheck 3 (ternLawsOn Set.fromList :: TernaryLaws [[WordN 2]])

  putStrLn "Min Integer"
  smallCheck 1000 (unLawsOn   Min :: UnaryLaws   (Maybe Integer))
  smallCheck 100  (binLawsOn  Min :: BinaryLaws  (Maybe Integer))
  smallCheck 10   (ternLawsOn Min :: TernaryLaws (Maybe Integer))

  putStrLn "Max Integer"
  smallCheck 1000 (unLawsOn   Max :: UnaryLaws   (Maybe Integer))
  smallCheck 100  (binLawsOn  Max :: BinaryLaws  (Maybe Integer))
  smallCheck 10   (ternLawsOn Max :: TernaryLaws (Maybe Integer))

  putStrLn "Free (WordN 2)"
  smallCheck 4 (unLawsOn   Free :: UnaryLaws   [[WordN 2]])
  smallCheck 3 (binLawsOn  Free :: BinaryLaws  [[WordN 2]])
  smallCheck 3 (ternLawsOn Free :: TernaryLaws [[WordN 2]])

  putStrLn "Bottleneck (WordN 2)"
  smallCheck 1000 (unLawsOn   Bottleneck :: UnaryLaws   (WordN 2))
  smallCheck 100  (binLawsOn  Bottleneck :: BinaryLaws  (WordN 2))
  smallCheck 10   (ternLawsOn Bottleneck :: TernaryLaws (WordN 2))

  putStrLn "Division Integer"
  smallCheck 1000 (unLawsOn   (Division . getPositive) :: UnaryLaws   (Positive Integer))
  smallCheck 100  (binLawsOn  (Division . getPositive) :: BinaryLaws  (Positive Integer))
  smallCheck 10   (ternLawsOn (Division . getPositive) :: TernaryLaws (Positive Integer))

  putStrLn "Łukasiewicz Double"
  smallCheck 1000 (unLawsOn   Łukasiewicz :: UnaryLaws   Fraction)
  smallCheck 100  (binLawsOn  Łukasiewicz :: BinaryLaws  Fraction)
  smallCheck 10   (ternLawsOn Łukasiewicz :: TernaryLaws Fraction)

  putStrLn "Viterbi Double"
  smallCheck 1000 (unLawsOn   Viterbi :: UnaryLaws   Fraction)
  smallCheck 100  (binLawsOn  Viterbi :: BinaryLaws  Fraction)
  smallCheck 10   (ternLawsOn Viterbi :: TernaryLaws Fraction)

  putStrLn "Log Double"
  smallCheck 1000 (unLawsOn   Log :: UnaryLaws   (Maybe Fraction))
  smallCheck 100  (binLawsOn  Log :: BinaryLaws  (Maybe Fraction))
  smallCheck 10   (ternLawsOn Log :: TernaryLaws (Maybe Fraction))

  putStrLn "Bool -> Bool"
  smallCheck 3 (unLawsOn   fromFunc :: UnaryLaws   (Bool -> Bool))
  smallCheck 2 (binLawsOn  fromFunc :: BinaryLaws  (Bool -> Bool))
  smallCheck 2 (ternLawsOn fromFunc :: TernaryLaws (Bool -> Bool))

  putStrLn "Endo (Add Bool)"
  smallCheck 3 (unOn plusId        eFromFunc :: UnaryLaws   (Bool -> Bool))
  smallCheck 3 (unOn mulId         eFromFunc :: UnaryLaws   (Bool -> Bool))
  smallCheck 2 (binLawsOn          eFromFunc :: BinaryLaws  (Bool -> Bool))
  smallCheck 2 (ternOn plusAssoc   eFromFunc :: TernaryLaws (Bool -> Bool))
  smallCheck 2 (ternOn mulAssoc    eFromFunc :: TernaryLaws (Bool -> Bool))
  smallCheck 2 (ternOn mulDistribR eFromFunc :: TernaryLaws (Bool -> Bool))

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

unOn :: UnaryLaws b -> (a -> b) -> UnaryLaws a
unOn = (.)

binOn :: BinaryLaws b -> (a -> b) -> BinaryLaws a
binOn = on

ternOn :: TernaryLaws b -> (a -> b) -> TernaryLaws a
ternOn t f x y z = t (f x) (f y) (f z)

unLawsOn :: (Eq b, Semiring b, Show b) => (a -> b) -> UnaryLaws a
unLawsOn = unOn unaryLaws

binLawsOn :: (Eq b, Semiring b, Show b) => (a -> b) -> BinaryLaws a
binLawsOn = binOn binaryLaws

ternLawsOn :: (Eq b, Semiring b, Show b) => (a -> b) -> TernaryLaws a
ternLawsOn = ternOn ternaryLaws

------------------------------------------------------------------------
-- Serial wrappers

-- | A type with a serial instance between zero and one
newtype Fraction
  = Fraction Double
  deriving (Show, Num, Fractional, Real, RealFrac, Floating, RealFloat, Semiring)

instance Eq Fraction where
  Fraction x == Fraction y = abs (x-y) < 0.011

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
    interleave _ _ = undefined

-- | A very small numeric type for exhaustiveness
newtype WordN (n :: Nat) = WordN { getWordN :: Word } deriving Show

mask :: KnownNat n => WordN n -> Word
mask x = shift 1 (fromInteger (natVal x)) - 1

trunc :: KnownNat n => WordN n -> WordN n
trunc v@(WordN x) = WordN (x .&. mask v)

instance KnownNat n => Bounded (WordN n) where
  minBound = WordN 0
  maxBound = res where res = WordN (mask res)

instance KnownNat n => Num (WordN n) where
  WordN x + WordN y = trunc (WordN (x + y))
  WordN x * WordN y = trunc (WordN (x * y))
  WordN x - WordN y = trunc (WordN (x - y))
  fromInteger x = trunc (WordN (fromInteger x))
  abs = id
  signum (WordN x) = WordN (signum x)

instance KnownNat n => Eq (WordN n) where
  (==) = (==) `on` getWordN . trunc

instance KnownNat n => Ord (WordN n) where
  compare = compare `on` getWordN . trunc

instance KnownNat n => Real (WordN n) where
  toRational = toRational . getWordN

instance KnownNat n => Enum (WordN n) where
  fromEnum = fromEnum . getWordN
  toEnum = trunc . WordN . toEnum

instance KnownNat n => Integral (WordN n) where
  toInteger = toInteger . getWordN
  quotRem (WordN x) (WordN y) = (WordN (quot x y), WordN (rem x y))

instance (Monad m, KnownNat n) => Serial m (WordN n) where
  series = generate (`take` [minBound..maxBound])

instance KnownNat n => Semiring (WordN n)

------------------------------------------------------------------------
-- Function Equality

-- | A representation of a function
data Func a b = Func b (IntMap b)
  deriving (Eq, Ord)

newtype EndoFunc a = EndoFunc (Endo a) deriving Semiring

instance (Enum a, Bounded a, Ord a) => Eq (EndoFunc a) where
  EndoFunc (Endo f) == EndoFunc (Endo g) = fromFunc f == fromFunc g

instance (Enum a, Bounded a, Ord a, Show a) => Show (EndoFunc a) where
  show (EndoFunc (Endo f)) = show (fromFunc f)

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

eFromFunc :: (a -> a) -> EndoFunc (Add a)
eFromFunc f = (EndoFunc . Endo) (Add . f . getAdd)

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
