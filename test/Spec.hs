{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Main (main) where

import           Control.Applicative

import           Control.Arrow            (first)
import           Data.Bool
import           Data.Proxy

import           Data.Foldable
import           Data.Monoid

import           Data.IntMap.Strict       (IntMap)
import qualified Data.IntMap.Strict       as IntMap

import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as Map

import           Data.Semiring
import           Data.Semiring.Free
import           Data.Semiring.Infinite
import           Data.Semiring.Numeric

import           GHC.TypeLits
import           Numeric.Natural
import           Numeric.Sized.WordOfSize

import           Test.DocTest
import           Test.QuickCheck          hiding (Positive (..), generate,
                                           (.&.))
import           Test.SmallCheck.Series   hiding (Positive)
import qualified Test.SmallCheck.Series   as SC
import           Test.Tasty
import qualified Test.Tasty.QuickCheck    as QC
import qualified Test.Tasty.SmallCheck    as SC

import           Test.Semiring

import           Data.Functor.Classes

------------------------------------------------------------------------

semiringLawsSC :: (Show r, Eq r, Semiring r, Serial IO r) => f r -> TestTree
semiringLawsSC (_ :: f r) = testGroup "Semiring Laws"
  [ SC.testProperty "plusId" (plusId :: r -> Either String String)
  , SC.testProperty "mulId" (mulId  :: r -> Either String String)
  , SC.testProperty "annihilateL" (annihilateL  :: r -> Either String String)
  , SC.testProperty "annihilateR" (annihilateR  :: r -> Either String String)
  , SC.testProperty "plusComm" (plusComm  :: r -> r -> Either String String)
  , SC.testProperty "plusAssoc" (plusAssoc  :: r -> r -> r -> Either String String)
  , SC.testProperty "mulAssoc" (mulAssoc  :: r -> r -> r -> Either String String)
  , SC.testProperty "mulDistribL" (mulDistribL  :: r -> r -> r -> Either String String)
  , SC.testProperty "mulDistribR" (mulDistribR  :: r -> r -> r -> Either String String)]

semiringLawsQC :: (Show r, Eq r, Semiring r, Arbitrary r) => f r -> TestTree
semiringLawsQC (_ :: f r) = testGroup "Semiring Laws"
  [ QC.testProperty "plusId" (plusId :: r -> Either String String)
  , QC.testProperty "mulId" (mulId  :: r -> Either String String)
  , QC.testProperty "annihilateL" (annihilateL  :: r -> Either String String)
  , QC.testProperty "annihilateR" (annihilateR  :: r -> Either String String)
  , QC.testProperty "plusComm" (plusComm  :: r -> r -> Either String String)
  , QC.testProperty "plusAssoc" (plusAssoc  :: r -> r -> r -> Either String String)
  , QC.testProperty "mulAssoc" (mulAssoc  :: r -> r -> r -> Either String String)
  , QC.testProperty "mulDistribL" (mulDistribL  :: r -> r -> r -> Either String String)
  , QC.testProperty "mulDistribR" (mulDistribR  :: r -> r -> r -> Either String String)]

starLawsQC :: (Show r, Eq r, StarSemiring r, Arbitrary r) => f r -> TestTree
starLawsQC (_ :: f r) = testGroup "Star laws"
  [ QC.testProperty "starLaw" (starLaw :: r -> Either String String)
  , QC.testProperty "plusLaw" (plusLaw :: r -> Either String String)]

starLawsSC :: (Show r, Eq r, StarSemiring r, Serial IO r) => f r -> TestTree
starLawsSC (_ :: f r) = testGroup "Star laws"
  [ SC.testProperty "starLaw" (starLaw :: r -> Either String String)
  , SC.testProperty "plusLaw" (plusLaw :: r -> Either String String)]

-- ordLawsQC :: (Show r, Ord r, Semiring r, Arbitrary r) => f r -> TestTree
-- ordLawsQC (_ :: f r) = testGroup "Ordering laws"
--   [ QC.testProperty "mulLaw" (ordMulLaw :: r -> r -> r -> Either String String)
--   , QC.testProperty "addLaw" (ordAddLaw :: r -> r -> r -> Either String String)]

zeroLawsQC :: (Show r, Eq r, DetectableZero r, Arbitrary r) => f r -> TestTree
zeroLawsQC (_ :: f r) = testGroup "Zero laws"
  [ QC.testProperty "zeroLaw" (zeroLaw :: r -> Either String String)
  , QC.testProperty "zeroIsZero" (once $ zeroIsZero (Proxy :: Proxy r))]

ordLawsSC :: (Show r, Ord r, Semiring r, Serial IO r) => f r -> TestTree
ordLawsSC (_ :: f r) = testGroup "Ordering laws"
  [ SC.testProperty "mulLaw" (ordMulLaw :: r -> r -> r -> Either String String)
  , SC.testProperty "addLaw" (ordAddLaw :: r -> r -> r -> Either String String)]

zeroLawsSC :: (Show r, Eq r, DetectableZero r, Serial IO r) => f r -> TestTree
zeroLawsSC (_ :: f r) = testGroup "Zero laws"
  [ SC.testProperty "zeroLaw" (zeroLaw :: r -> Either String String)
  , SC.testProperty "zeroIsZero" (zeroIsZero (Proxy :: Proxy r))]

type Tup2 a = (a,a)
type Tup3 a = (a,a,a)
type Tup4 a = (a,a,a,a)
type Tup5 a = (a,a,a,a,a)
type Tup6 a = (a,a,a,a,a,a)
type Tup7 a = (a,a,a,a,a,a,a)
type Tup8 a = (a,a,a,a,a,a,a,a)
type Tup9 a = (a,a,a,a,a,a,a,a,a)

main :: IO ()
main = do
    doctest ["-isrc", "src/"]
    defaultMain $
        testGroup
            "Tests"
            [ let p = Proxy :: Proxy (Map String Int)
              in testGroup "Map" [localOption (QC.QuickCheckMaxSize 10) $ semiringLawsQC p]
            , let p = Proxy :: Proxy (Matrix Quad Integer)
              in testGroup "Matrix" [semiringLawsQC p]
            , let p = Proxy :: Proxy Integer
              in testGroup
                     "Integer"
                     [semiringLawsSC p, ordLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (Func Bool Bool)
              in testGroup "Bool -> Bool"
                     [semiringLawsQC p]
            , testGroup "Endo Bool"
                  [ QC.testProperty
                        "plusId"
                        (plusId :: UnaryLaws (EndoFunc (Add Bool)))
                  , QC.testProperty
                        "mulId"
                        (mulId :: UnaryLaws (EndoFunc (Add Bool)))
                  , QC.testProperty
                        "annihilateR"
                        (annihilateR :: UnaryLaws (EndoFunc (Add Bool)))
                  , zeroLawsQC (Proxy :: Proxy (EndoFunc (Add Bool)))
                  , QC.testProperty
                        "plusComm"
                        (plusComm :: BinaryLaws (EndoFunc (Add Bool)))
                  , QC.testProperty
                        "plusAssoc"
                        (plusAssoc :: TernaryLaws (EndoFunc (Add Bool)))
                  , QC.testProperty
                        "mulAssoc"
                        (mulAssoc :: TernaryLaws (EndoFunc (Add Bool)))
                  , QC.testProperty
                        "mulDistribR"
                        (mulDistribR :: TernaryLaws (EndoFunc (Add Bool)))]
            , let p = Proxy :: Proxy (PositiveInfinite Natural)
              in testGroup
                     "PosInf Natural"
                     [semiringLawsSC p, ordLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy Int
              in testGroup "Int" [semiringLawsSC p, ordLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (WordOfSize 2)
              in testGroup
                     "WordOfSize 2"
                     [semiringLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (Tup2 (WordOfSize 2))
              in testGroup
                     "Tup2 (WordOfSize 2)"
                     [semiringLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (Tup3 (WordOfSize 2))
              in testGroup
                     "Tup3 (WordOfSize 2)"
                     [semiringLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (Tup4 Int)
              in testGroup
                     "Tup4 Int"
                     [semiringLawsQC p, zeroLawsQC p]
            , let p = Proxy :: Proxy (Tup5 Int)
              in testGroup
                     "Tup5 Int"
                     [semiringLawsQC p, zeroLawsQC p]
            , let p = Proxy :: Proxy (Tup6 Int)
              in testGroup
                     "Tup6 Int"
                     [semiringLawsQC p, zeroLawsQC p]
            , let p = Proxy :: Proxy (Tup7 Int)
              in testGroup
                     "Tup7 Int"
                     [semiringLawsQC p, zeroLawsQC p]
            , let p = Proxy :: Proxy (Tup8 Int)
              in testGroup
                     "Tup8 Int"
                     [semiringLawsQC p, zeroLawsQC p]
            , let p = Proxy :: Proxy (Tup9 Int)
              in testGroup
                     "Tup9 Int"
                     [semiringLawsQC p, zeroLawsQC p]
            , let p = Proxy :: Proxy (Tup2 (PositiveInfinite (WordOfSize 2)))
              in testGroup
                     "Tup2 (WordOfSize 2)"
                     [starLawsSC p]
            , let p = Proxy :: Proxy (Tup3 (PositiveInfinite (WordOfSize 2)))
              in testGroup
                     "Tup3 (WordOfSize 2)"
                     [starLawsSC p]
            , let p = Proxy :: Proxy (Tup4 (PositiveInfinite Int))
              in testGroup
                     "Tup4 Int"
                     [starLawsQC p]
            , let p = Proxy :: Proxy (Tup5 (PositiveInfinite Int))
              in testGroup
                     "Tup5 Int"
                     [starLawsQC p]
            , let p = Proxy :: Proxy (Tup6 (PositiveInfinite Int))
              in testGroup
                     "Tup6 Int"
                     [starLawsQC p]
            , let p = Proxy :: Proxy (Tup7 (PositiveInfinite Int))
              in testGroup
                     "Tup7 Int"
                     [starLawsQC p]
            , let p = Proxy :: Proxy (Tup8 (PositiveInfinite Int))
              in testGroup
                     "Tup8 Int"
                     [starLawsQC p]
            , let p = Proxy :: Proxy (Tup9 (PositiveInfinite Int))
              in testGroup
                     "Tup9 Int"
                     [starLawsQC p]
            , testGroup
                  "Negative Infinite Integer"
                  [ SC.testProperty
                        "plusId"
                        (plusId :: UnaryLaws (NegativeInfinite Integer))
                  , SC.testProperty
                        "mulId"
                        (mulId :: UnaryLaws (NegativeInfinite Integer))
                  , SC.testProperty
                        "annihilateR"
                        (annihilateR :: UnaryLaws (NegativeInfinite Integer))
                  , zeroLawsSC (Proxy :: Proxy (NegativeInfinite Integer))
                  , SC.testProperty
                        "plusComm"
                        (plusComm :: BinaryLaws (NegativeInfinite Integer))
                  , ordLawsSC (Proxy :: Proxy (NegativeInfinite Integer))
                  , SC.testProperty
                        "plusAssoc"
                        (plusAssoc :: TernaryLaws (NegativeInfinite Integer))
                  , SC.testProperty
                        "mulAssoc"
                        (mulAssoc :: TernaryLaws (NegativeInfinite Integer))
                  , SC.testProperty
                        "mulDistribL"
                        (mulDistribL :: TernaryLaws (NegativeInfinite Integer))]
            , testGroup
                  "Infinite Integer"
                  [ SC.testProperty
                        "plusId"
                        (plusId :: UnaryLaws (Infinite Integer))
                  , SC.testProperty
                        "mulId"
                        (mulId :: UnaryLaws (Infinite Integer))
                  , SC.testProperty
                        "annihilateR"
                        (annihilateR :: UnaryLaws (Infinite Integer))
                  , SC.testProperty
                        "annihilateL"
                        (annihilateL :: UnaryLaws (Infinite Integer))
                  , zeroLawsSC (Proxy :: Proxy (Infinite Integer))
                  , SC.testProperty
                        "plusComm"
                        (plusComm :: BinaryLaws (Infinite Integer))
                  , ordLawsSC (Proxy :: Proxy (Infinite Integer))
                  , SC.testProperty
                        "plusAssoc"
                        (plusAssoc :: TernaryLaws (Infinite Integer))
                  , SC.testProperty
                        "mulAssoc"
                        (mulAssoc :: TernaryLaws (Infinite Integer))]
            , let p = Proxy :: Proxy ()
              in testGroup
                     "()"
                     [semiringLawsSC p, ordLawsSC p, zeroLawsSC p, starLawsSC p]
            , let p = Proxy :: Proxy Bool
              in testGroup
                     "Bool"
                     [semiringLawsSC p, ordLawsSC p, zeroLawsSC p, starLawsSC p]
            , let p = Proxy :: Proxy Any
              in testGroup
                     "Any"
                     [semiringLawsSC p, ordLawsSC p, zeroLawsSC p, starLawsSC p]
            , let p = Proxy :: Proxy All
              in testGroup
                     "All"
                     [semiringLawsSC p, ordLawsSC p, zeroLawsSC p, starLawsSC p]
            , let p = Proxy :: Proxy [WordOfSize 2]
              in testGroup "[WordOfSize 2]" [semiringLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (Min (PositiveInfinite Integer))
              in testGroup
                     "Min Inf Integer"
                     [semiringLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (Min (Infinite Integer))
              in testGroup
                     "Min Inf Integer"
                     [starLawsSC p]
            , let p = Proxy :: Proxy (Max (NegativeInfinite Integer))
              in testGroup
                     "Max NegInf Integer"
                     [semiringLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (Max (Infinite Integer))
              in testGroup
                     "Max Inf Integer"
                     [starLawsSC p]
            , let p = Proxy :: Proxy (Free (WordOfSize 2))
              in testGroup "Free (WordOfSize 2)" [localOption (QC.QuickCheckMaxSize 10) $ semiringLawsQC p]
            , let p = Proxy :: Proxy (Division (SC.Positive Integer))
              in testGroup "Division Integer" [semiringLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (Łukasiewicz Fraction)
              in testGroup
                     "Łukasiewicz Fraction"
                     [semiringLawsSC p, zeroLawsSC p]
            , let p = Proxy :: Proxy (Viterbi Fraction)
              in testGroup "Viterbi Fraction" [semiringLawsSC p, zeroLawsSC p]]
            -- , let p = Proxy :: Proxy (Log (Approx Double))
            --   in testGroup
            --          "Log (Approx Double)"
            --          [semiringLawsQC p, zeroLawsQC p]]


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

instance KnownNat n => Semiring (WordOfSize n) where
  one = 1
  zero = 0
  (<+>) = (+)
  (<.>) = (*)

instance KnownNat n => DetectableZero (WordOfSize n) where
  isZero = (zero==)

instance (Monad m, Serial m a) => Serial m (PositiveInfinite a) where
  series = fmap (maybe PositiveInfinity PosFinite) series

instance (Monad m, Serial m a) => Serial m (NegativeInfinite a) where
  series = fmap (maybe NegativeInfinity NegFinite) series

instance (Monad m, Serial m a) => Serial m (Infinite a) where
  series = fmap (either (bool Positive Negative) Finite) series

instance Monad m => Serial m Natural where
  series = generate (`take` [0..])

instance Monad m => Serial m Any where
  series = fmap Any series

instance Monad m => Serial m All where
  series = fmap All series

instance (Monad m, Serial m a) => Serial m (Min a) where
  series = fmap Min series

instance (Monad m, Serial m a) => Serial m (Max a) where
  series = fmap Max series

instance (Ord a, Arbitrary a) => Arbitrary (Free a) where
  arbitrary = fmap Free arbitrary

instance Num a => Semiring (SC.Positive a) where
  zero = 0
  one = 1
  (<+>) = (+)
  (<.>) = (*)

instance (Eq a, Num a) => DetectableZero (SC.Positive a) where
  isZero = (zero==)

instance (Serial m a, Monad m) => Serial m (Division a) where
  series = fmap Division series

instance (Serial m a, Monad m) => Serial m (Łukasiewicz a) where
  series = fmap Łukasiewicz series

instance (Serial m a, Monad m) => Serial m (Viterbi a) where
  series = fmap Viterbi series

-- instance (Serial m a, Monad m) => Serial m (Log a) where
--   series = fmap Log series

-- instance Arbitrary a => Arbitrary (Log a) where
--   arbitrary = fmap Log arbitrary

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

instance (Bounded a, Enum a, Ord b, Arbitrary b, CoArbitrary a) =>
         Arbitrary (Func a b) where
    arbitrary = fmap fromFunc arbitrary

instance (Arbitrary a, CoArbitrary a) =>
         Arbitrary (EndoFunc (Add a)) where
    arbitrary = fmap eFromFunc arbitrary

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

data Pair a b = !a :*: !b

fst' :: Pair a b -> a
fst' (x :*: _) = x

data Many a = (:#:) {-# UNPACK #-} !Int !a

val :: Many a -> a
val (_ :#: x) = x

mostFrequent :: (Ord a, Foldable f) => f a -> Maybe a
mostFrequent = fmap val . fst' . foldl' f (Nothing :*: (Map.empty :: Map a Int)) where
  f (b :*: m) e = Just nb :*: Map.insert e c m where
    c = maybe 1 succ (Map.lookup e m)
    nb = case b of
      Just (d :#: a) | d >= c -> d :#: a
      _              -> c :#: e

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

data Quad a = Quad a a a a deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance Applicative Quad where
    pure x = Quad x x x x
    Quad fw fx fy fz <*> Quad xw xx xy xz = Quad (fw xw) (fx xx) (fy xy) (fz xz)

instance Eq1 Quad where
    liftEq eq x y = mulFoldable (liftA2 eq x y)

instance Ord1 Quad where
    liftCompare cmp x y = fold (liftA2 cmp x y)

instance Show1 Quad where
    liftShowsPrec sp _ n (Quad w x y z) =
        showParen (n > 10) $
        showString "Quad " .
        sp 10 w . sp 10 x . sp 10 y . sp 10 z

instance Arbitrary a => Arbitrary (Quad a) where
    arbitrary = Quad <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    shrink = traverse shrink

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

instance Arbitrary (f (f a)) => Arbitrary (Matrix f a) where
    arbitrary = fmap Matrix arbitrary
    shrink (Matrix xs) = fmap Matrix (shrink xs)
