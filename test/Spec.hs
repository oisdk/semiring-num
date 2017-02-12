{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Main (main) where

import           Control.Applicative
import           Control.Arrow            (first)
import           Data.Bool
import           Data.Foldable
import           Data.Function
import           Data.IntMap.Strict       (IntMap)
import qualified Data.IntMap.Strict       as IntMap
import qualified Data.Map.Strict          as Map
import           Data.Monoid
import           Data.Semiring
import           Data.Semiring.Free
import           Data.Semiring.Infinite
import           Data.Semiring.Numeric
import           GHC.TypeLits
import           Numeric.Sized.WordOfSize
import           Test.DocTest
import           Test.QuickCheck          hiding (Positive (..), generate,
                                           (.&.))
import           Test.Semiring
import           Test.SmallCheck          hiding (Testable, (==>))
import           Test.SmallCheck.Series   hiding (Positive)
import qualified Test.SmallCheck.Series   as SC

import Numeric.Natural

------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Integer"
  smallCheck 1000 (unaryLaws   :: UnaryLaws   Integer)
  smallCheck 1000 (zeroLaws    :: UnaryLaws   Integer)
  smallCheck 100  (binaryLaws  :: BinaryLaws  Integer)
  smallCheck 10   (ternaryLaws :: TernaryLaws Integer)
  smallCheck 10   (ordLaws     :: TernaryLaws Integer)

  putStrLn "(WordOfSize 2)"
  smallCheck 16  (unaryLaws   :: UnaryLaws   (WordOfSize 2))
  smallCheck 16  (zeroLaws    :: UnaryLaws   (WordOfSize 2))
  smallCheck 16  (binaryLaws  :: BinaryLaws  (WordOfSize 2))
  smallCheck 16  (ternaryLaws :: TernaryLaws (WordOfSize 2))
  smallCheck 16  (starLaws    :: UnaryLaws   (PositiveInfinite (WordOfSize 2)))

  putStrLn "(WordOfSize 2,WordOfSize 2)"
  smallCheck 16 (unaryLaws   :: UnaryLaws   (WordOfSize 2,WordOfSize 2))
  smallCheck 16 (zeroLaws    :: UnaryLaws   (WordOfSize 2,WordOfSize 2))
  smallCheck 14 (binaryLaws  :: BinaryLaws  (WordOfSize 2,WordOfSize 2))
  smallCheck 8  (ternaryLaws :: TernaryLaws (WordOfSize 2,WordOfSize 2))
  smallCheck 16 (starLaws    :: UnaryLaws   (PositiveInfinite (WordOfSize 2)
                                            ,PositiveInfinite (WordOfSize 2)))

  putStrLn "(WordOfSize 2,WordOfSize 2,WordOfSize 2)"
  smallCheck 10 (unaryLaws   :: UnaryLaws   (WordOfSize 2,WordOfSize 2,WordOfSize 2))
  smallCheck 10 (zeroLaws    :: UnaryLaws   (WordOfSize 2,WordOfSize 2,WordOfSize 2))
  smallCheck 5  (binaryLaws  :: BinaryLaws  (WordOfSize 2,WordOfSize 2,WordOfSize 2))
  smallCheck 2  (ternaryLaws :: TernaryLaws (WordOfSize 2,WordOfSize 2,WordOfSize 2))
  smallCheck 10 (starLaws    :: UnaryLaws   (PositiveInfinite (WordOfSize 2)
                                            ,PositiveInfinite (WordOfSize 2)
                                            ,PositiveInfinite (WordOfSize 2)))

  putStrLn "(WordOfSize 2,WordOfSize 2,WordOfSize 2,WordOfSize 2)"
  smallCheck 8 (unaryLaws   :: UnaryLaws   (WordOfSize 2,WordOfSize 2,WordOfSize 2,WordOfSize 2))
  smallCheck 8 (zeroLaws    :: UnaryLaws   (WordOfSize 2,WordOfSize 2,WordOfSize 2,WordOfSize 2))
  smallCheck 4 (binaryLaws  :: BinaryLaws  (WordOfSize 2,WordOfSize 2,WordOfSize 2,WordOfSize 2))
  smallCheck 1 (ternaryLaws :: TernaryLaws (WordOfSize 2,WordOfSize 2,WordOfSize 2,WordOfSize 2))
  smallCheck 16 (starLaws    :: UnaryLaws   (PositiveInfinite (WordOfSize 2)
                                            ,PositiveInfinite (WordOfSize 2)
                                            ,PositiveInfinite (WordOfSize 2)
                                            ,PositiveInfinite (WordOfSize 2)))

  putStrLn "(Int,Int,Int,Int,Int)"
  quickCheck (unaryLaws   :: UnaryLaws   (Int,Int,Int,Int,Int))
  quickCheck (zeroLaws    :: UnaryLaws   (Int,Int,Int,Int,Int))
  quickCheck (binaryLaws  :: BinaryLaws  (Int,Int,Int,Int,Int))
  quickCheck (ternaryLaws :: TernaryLaws (Int,Int,Int,Int,Int))
  quickCheck (starLaws    :: UnaryLaws   (PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int))

  putStrLn "(Int,Int,Int,Int,Int,Int)"
  quickCheck (unaryLaws   :: UnaryLaws   (Int,Int,Int,Int,Int,Int))
  quickCheck (zeroLaws    :: UnaryLaws   (Int,Int,Int,Int,Int,Int))
  quickCheck (binaryLaws  :: BinaryLaws  (Int,Int,Int,Int,Int,Int))
  quickCheck (ternaryLaws :: TernaryLaws (Int,Int,Int,Int,Int,Int))
  quickCheck (starLaws    :: UnaryLaws   (PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int))

  putStrLn "(Int,Int,Int,Int,Int,Int,Int)"
  quickCheck (unaryLaws   :: UnaryLaws   (Int,Int,Int,Int,Int,Int,Int))
  quickCheck (zeroLaws    :: UnaryLaws   (Int,Int,Int,Int,Int,Int,Int))
  quickCheck (binaryLaws  :: BinaryLaws  (Int,Int,Int,Int,Int,Int,Int))
  quickCheck (ternaryLaws :: TernaryLaws (Int,Int,Int,Int,Int,Int,Int))
  quickCheck (starLaws    :: UnaryLaws   (PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int))

  putStrLn "(Int,Int,Int,Int,Int,Int,Int,Int)"
  quickCheck (unaryLaws   :: UnaryLaws   (Int,Int,Int,Int,Int,Int,Int,Int))
  quickCheck (zeroLaws    :: UnaryLaws   (Int,Int,Int,Int,Int,Int,Int,Int))
  quickCheck (binaryLaws  :: BinaryLaws  (Int,Int,Int,Int,Int,Int,Int,Int))
  quickCheck (ternaryLaws :: TernaryLaws (Int,Int,Int,Int,Int,Int,Int,Int))
  quickCheck (starLaws    :: UnaryLaws   (PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int))

  putStrLn "(Int,Int,Int,Int,Int,Int,Int,Int,Int)"
  quickCheck (unaryLaws   :: UnaryLaws   (Int,Int,Int,Int,Int,Int,Int,Int,Int))
  quickCheck (zeroLaws    :: UnaryLaws   (Int,Int,Int,Int,Int,Int,Int,Int,Int))
  quickCheck (binaryLaws  :: BinaryLaws  (Int,Int,Int,Int,Int,Int,Int,Int,Int))
  quickCheck (ternaryLaws :: TernaryLaws (Int,Int,Int,Int,Int,Int,Int,Int,Int))
  quickCheck (starLaws    :: UnaryLaws   (PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int
                                         ,PositiveInfinite Int))

  putStrLn "Int"
  smallCheck 1000 (unaryLaws   :: UnaryLaws   Int)
  smallCheck 1000 (zeroLaws    :: UnaryLaws   Int)
  smallCheck 100  (binaryLaws  :: BinaryLaws  Int)
  smallCheck 10   (ternaryLaws :: TernaryLaws Int)

  putStrLn "PosInf Natural"
  smallCheck 1000 (unaryLaws   :: UnaryLaws   (PositiveInfinite Natural))
  smallCheck 1000 (zeroLaws    :: UnaryLaws   (PositiveInfinite Natural))
  smallCheck 100  (binaryLaws  :: BinaryLaws  (PositiveInfinite Natural))
  smallCheck 10   (ternaryLaws :: TernaryLaws (PositiveInfinite Natural))
  smallCheck 10   (ordLaws     :: TernaryLaws (PositiveInfinite Natural))

  putStrLn "NegInf Integer"
  smallCheck 1000 (nearUnaryLaws   :: UnaryLaws   (NegativeInfinite Integer))
  smallCheck 1000 (zeroLaws        :: UnaryLaws   (NegativeInfinite Integer))
  smallCheck 100  (binaryLaws      :: BinaryLaws  (NegativeInfinite Integer))
  smallCheck 10   (nearTernaryLaws :: TernaryLaws (NegativeInfinite Integer))
  smallCheck 10   (ordLaws         :: TernaryLaws (NegativeInfinite Integer))

  putStrLn "Inf Integer"
  smallCheck 1000 (unaryLaws   :: UnaryLaws   (Infinite Integer))
  smallCheck 1000 (zeroLaws    :: UnaryLaws   (Infinite Integer))
  smallCheck 100  (binaryLaws  :: BinaryLaws  (Infinite Integer))
  smallCheck 10   (plusAssoc   :: TernaryLaws (Infinite Integer))
  smallCheck 10   (mulAssoc    :: TernaryLaws (Infinite Integer))
  smallCheck 10   (ordLaws     :: TernaryLaws (Infinite Integer))

  putStrLn "()"
  smallCheck 1 (unaryLaws   :: UnaryLaws   ())
  smallCheck 1 (zeroLaws    :: UnaryLaws   ())
  smallCheck 1 (binaryLaws  :: BinaryLaws  ())
  smallCheck 1 (ternaryLaws :: TernaryLaws ())
  smallCheck 1 (starLaws    :: UnaryLaws   ())

  putStrLn "Bool"
  smallCheck 2 (unaryLaws   :: UnaryLaws   Bool)
  smallCheck 2 (zeroLaws    :: UnaryLaws   Bool)
  smallCheck 4 (binaryLaws  :: BinaryLaws  Bool)
  smallCheck 8 (ternaryLaws :: TernaryLaws Bool)
  smallCheck 2 (starLaws    :: UnaryLaws   Bool)

  putStrLn "Any"
  smallCheck 2 (unLawsOn   Any :: UnaryLaws   Bool)
  smallCheck 2 (zeroLaws . Any :: UnaryLaws   Bool)
  smallCheck 4 (binLawsOn  Any :: BinaryLaws  Bool)
  smallCheck 8 (ternLawsOn Any :: TernaryLaws Bool)

  putStrLn "All"
  smallCheck 2 (unLawsOn   All :: UnaryLaws   Bool)
  smallCheck 2 (zeroLaws . All :: UnaryLaws   Bool)
  smallCheck 4 (binLawsOn  All :: BinaryLaws  Bool)
  smallCheck 8 (ternLawsOn All :: TernaryLaws Bool)

  putStrLn "[WordOfSize 2]"
  smallCheck 5 (unaryLaws   :: UnaryLaws   [WordOfSize 2])
  smallCheck 5 (zeroLaws    :: UnaryLaws   [WordOfSize 2])
  smallCheck 4 (binaryLaws  :: BinaryLaws  [WordOfSize 2])
  smallCheck 3 (ternaryLaws :: TernaryLaws [WordOfSize 2])

  putStrLn "Min Integer"
  smallCheck 1000 (unLawsOn   Min :: UnaryLaws   (PositiveInfinite Integer))
  smallCheck 100  (binLawsOn  Min :: BinaryLaws  (PositiveInfinite Integer))
  smallCheck 10   (ternLawsOn Min :: TernaryLaws (PositiveInfinite Integer))
  smallCheck 1000 (starLaws . Min :: UnaryLaws   (Infinite    Integer))

  putStrLn "Max Integer"
  smallCheck 1000 (unLawsOn   Max :: UnaryLaws   (NegativeInfinite Integer))
  smallCheck 100  (binLawsOn  Max :: BinaryLaws  (NegativeInfinite Integer))
  smallCheck 10   (ternLawsOn Max :: TernaryLaws (NegativeInfinite Integer))
  smallCheck 1000 (starLaws . Max :: UnaryLaws   (Infinite    Integer))

  putStrLn "Free (WordOfSize 2)"
  smallCheck 4 (unLawsOn   Free :: UnaryLaws   [[WordOfSize 2]])
  smallCheck 3 (binLawsOn  Free :: BinaryLaws  [[WordOfSize 2]])
  smallCheck 3 (ternLawsOn Free :: TernaryLaws [[WordOfSize 2]])

  putStrLn "Bottleneck (WordOfSize 2)"
  smallCheck 1000 (unLawsOn   Bottleneck :: UnaryLaws   (WordOfSize 2))
  smallCheck 1000 (zeroLaws . Bottleneck :: UnaryLaws   (WordOfSize 2))
  smallCheck 100  (binLawsOn  Bottleneck :: BinaryLaws  (WordOfSize 2))
  smallCheck 10   (ternLawsOn Bottleneck :: TernaryLaws (WordOfSize 2))

  putStrLn "Division Integer"
  smallCheck 1000 (unLawsOn   (Division . getPositive) :: UnaryLaws   (SC.Positive Integer))
  smallCheck 1000 (zeroLaws .  Division . getPositive  :: UnaryLaws   (SC.Positive Integer))
  smallCheck 100  (binLawsOn  (Division . getPositive) :: BinaryLaws  (SC.Positive Integer))
  smallCheck 10   (ternLawsOn (Division . getPositive) :: TernaryLaws (SC.Positive Integer))

  putStrLn "Łukasiewicz Double"
  smallCheck 1000 (unLawsOn   Łukasiewicz :: UnaryLaws   Fraction)
  smallCheck 1000 (zeroLaws . Łukasiewicz :: UnaryLaws   Fraction)
  smallCheck 100  (binLawsOn  Łukasiewicz :: BinaryLaws  Fraction)
  smallCheck 10   (ternLawsOn Łukasiewicz :: TernaryLaws Fraction)

  putStrLn "Viterbi Double"
  smallCheck 1000 (unLawsOn   Viterbi :: UnaryLaws   Fraction)
  smallCheck 1000 (zeroLaws . Viterbi :: UnaryLaws   Fraction)
  smallCheck 100  (binLawsOn  Viterbi :: BinaryLaws  Fraction)
  smallCheck 10   (ternLawsOn Viterbi :: TernaryLaws Fraction)

  putStrLn "Log Double"
  quickCheck (unLawsOn   Log :: UnaryLaws   (Approx Double))
  quickCheck (zeroLaws . Log :: UnaryLaws   (Approx Double))
  quickCheck (binLawsOn  Log :: BinaryLaws  (Approx Double))
  quickCheck (ternLawsOn Log :: TernaryLaws (Approx Double))

  putStrLn "Bool -> Bool"
  smallCheck 3 (unLawsOn   fromFunc :: UnaryLaws   (Bool -> Bool))
  smallCheck 2 (binLawsOn  fromFunc :: BinaryLaws  (Bool -> Bool))
  smallCheck 2 (ternLawsOn fromFunc :: TernaryLaws (Bool -> Bool))
  quickCheck (unLawsOn   fromFunc :: UnaryLaws   (Bool -> Bool))
  quickCheck (binLawsOn  fromFunc :: BinaryLaws  (Bool -> Bool))
  quickCheck (ternLawsOn fromFunc :: TernaryLaws (Bool -> Bool))


  putStrLn "Endo (Add Bool)"
  smallCheck 3 (unOn plusId        eFromFunc :: UnaryLaws   (Bool -> Bool))
  smallCheck 3 (zeroLaws .         eFromFunc :: UnaryLaws   (Bool -> Bool))
  smallCheck 3 (unOn mulId         eFromFunc :: UnaryLaws   (Bool -> Bool))
  smallCheck 2 (binLawsOn          eFromFunc :: BinaryLaws  (Bool -> Bool))
  smallCheck 2 (ternOn plusAssoc   eFromFunc :: TernaryLaws (Bool -> Bool))
  smallCheck 2 (ternOn mulAssoc    eFromFunc :: TernaryLaws (Bool -> Bool))
  smallCheck 2 (ternOn mulDistribR eFromFunc :: TernaryLaws (Bool -> Bool))

  doctest [ "-isrc"
          , "src/" ]


-- Test helpers

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
newtype Fraction =
    Fraction Double
    deriving (Show,Num,Fractional,Real,RealFrac,Floating,RealFloat,Semiring)

instance DetectableZero Fraction where isZero = (0==)

newtype Approx a =
    Approx a
    deriving (Show,Num,Fractional,Real,RealFrac,Floating,RealFloat,Semiring
             ,HasPositiveInfinity)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (Approx a) where
  arbitrary = fmap Approx (suchThat arbitrary ((<100).abs))

instance Eq Fraction where
    Fraction x == Fraction y = abs (x - y) < 0.011

instance (RealFloat a, Ord a) =>
         Eq (Approx a) where
    Approx x == Approx y =
        isInfinite x && isInfinite y ||
        x == y ||
        let n = abs (x - y)
        in max (n / abs x) (n / abs y) < 0.011

instance (RealFloat a, Ord a) => Ord (Approx a) where
    compare (Approx x) (Approx y)
      | Approx x == Approx y = EQ
      | otherwise = compare x y

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

instance (Monad m, KnownNat n) => Serial m (WordOfSize n) where
  series = generate (`take` [minBound..maxBound])

instance KnownNat n => Arbitrary (WordOfSize n) where
  arbitrary = arbitraryBoundedEnum

instance KnownNat n => Semiring (WordOfSize n)
instance KnownNat n => DetectableZero (WordOfSize n)

instance (Monad m, Serial m a) => Serial m (PositiveInfinite a) where
  series = fmap (maybe PositiveInfinity PosFinite) series

instance (Monad m, Serial m a) => Serial m (NegativeInfinite a) where
  series = fmap (maybe NegativeInfinity NegFinite) series

instance (Monad m, Serial m a) => Serial m (Infinite a) where
  series = fmap (either (bool Positive Negative) Finite) series

instance Monad m => Serial m Natural where
  series = generate (`take` [0..])

------------------------------------------------------------------------
-- Function Equality

-- | A representation of a function
data Func a b = Func b (IntMap b)
  deriving (Eq, Ord)

newtype EndoFunc a = EndoFunc (Endo a) deriving (Semiring, DetectableZero)

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
      _          -> (e,c)

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



------------------------------------------------------------------------
-- QuickCheck wrappers

instance Arbitrary a => Arbitrary (PositiveInfinite a) where
  arbitrary = fmap (maybe PositiveInfinity PosFinite) arbitrary

instance Arbitrary a => Arbitrary (NegativeInfinite a) where
  arbitrary = fmap (maybe NegativeInfinity NegFinite) arbitrary

instance Arbitrary a => Arbitrary (Infinite a) where
  arbitrary = fmap (either (bool Positive Negative) Finite) arbitrary

instance Testable (Either String String) where
  property = either (`counterexample` False) (const (property True))

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         ,Arbitrary f)
  => Arbitrary (a,b,c,d,e,f) where
    arbitrary = (,,,,,) <$> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary
                        <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         ,Arbitrary f, Arbitrary g)
  => Arbitrary (a,b,c,d,e,f,g) where
    arbitrary = (,,,,,,) <$> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary
                         <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         ,Arbitrary f, Arbitrary g, Arbitrary h)
  => Arbitrary (a,b,c,d,e,f,g,h) where
    arbitrary = (,,,,,,,) <$> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary
                          <*> arbitrary

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e
         ,Arbitrary f, Arbitrary g, Arbitrary h, Arbitrary i)
  => Arbitrary (a,b,c,d,e,f,g,h,i) where
    arbitrary = (,,,,,,,,) <$> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
                           <*> arbitrary
