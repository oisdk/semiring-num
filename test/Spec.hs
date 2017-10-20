{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}

module Main (main) where

import           Data.Proxy

import           Data.Monoid

import           Data.Map.Strict          (Map)

import qualified Data.Vector              as Vector
import qualified Data.Vector.Storable     as Storable
import qualified Data.Vector.Unboxed      as Unboxed

import           Data.Semiring
import           Data.Semiring.Free
import           Data.Semiring.Infinite
import           Data.Semiring.Numeric

import           Data.Functor.Classes

import           Numeric.Natural
import           Numeric.Sized.WordOfSize

import           Test.DocTest
import           Test.QuickCheck          hiding (Positive (..), generate,
                                           (.&.))
import           Test.QuickCheck.Poly
import           Test.SmallCheck.Series   hiding (Positive)
import           Test.Tasty
import qualified Test.Tasty.QuickCheck    as QC
import qualified Test.Tasty.SmallCheck    as SC

import           Test.Semiring

import           ApproxLog
import           Fraction
import           Func
import           LimitSize
import           Orphans                  ()
import           Vectors

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

ordLawsQC :: (Show r, Ord r, Semiring r, Arbitrary r) => f r -> TestTree
ordLawsQC (_ :: f r) = testGroup "Ordering laws"
  [ QC.testProperty "mulLaw" (ordMulLaw :: r -> r -> r -> Either String String)
  , QC.testProperty "addLaw" (ordAddLaw :: r -> r -> r -> Either String String)]

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

storableQC :: (Show r, Eq r, Arbitrary r, Storable.Storable r) => f r -> TestTree
storableQC (_ :: f r) =
    testGroup
        "Storable implementation"
        [ QC.testProperty
              "unstore . store == id"
              (\(xs :: [r]) ->
                    (Storable.toList |.| Storable.fromList) xs === xs)]

infixr 9 |.|
(|.|) :: (b -> c) -> (a -> b) -> a -> c
(|.|) f g x = f (g x)
{-# NOINLINE (|.|) #-}

deriving instance Ord A

instance Read A where
    readsPrec p xs =
        [ (A x, rs)
        | (x,rs) <- readsPrec p xs ]

liftedQC
    :: (Show1 r
       ,Eq1 r
       ,Ord1 r
       ,Read1 r
       ,Arbitrary (r A)
       ,Show (r A)
       ,Eq (r A)
       ,Ord (r A)
       ,Read A)
    => f (r b) -> TestTree
liftedQC (_ :: f (r b)) =
    testGroup
        "liftedClasses"
        [ testGroup
              "Eq1"
              [ QC.testProperty
                    "x == x"
                    (\(x :: r A) ->
                          eq1 x x)
              , QC.testProperty
                    "same as =="
                    (\(x :: r A) (y :: r A) ->
                          counterexample (show (x, y)) ((x == y) == eq1 x y))]
        , testGroup
              "Ord1"
              [ QC.testProperty
                    "cmp x x == EQ"
                    (\(x :: r A) ->
                          counterexample (show x) (compare1 x x === EQ))
              , QC.testProperty
                    "compare1 == compare"
                    (\(x :: r A) (y :: r A) ->
                          counterexample
                              (show (x, y))
                              (compare x y == compare1 x y))]
        , testGroup
              "Show1"
              [ QC.testProperty
                    "show1 == show"
                    (\(x :: r A) ->
                          liftShowsPrec showsPrec showList 0 x "" === show x)]
        , testGroup
              "Read1"
              [ QC.testProperty
                    "read1 . show == id"
                    (\(x :: r A) ->
                          (liftReadsPrec readsPrec readList 0 . show) x ===
                          [(x, "")])]]

type Tup2 a = (a,a)
type Tup3 a = (a,a,a)
type Tup4 a = (a,a,a,a)
type Tup5 a = (a,a,a,a,a)
type Tup6 a = (a,a,a,a,a,a)
type Tup7 a = (a,a,a,a,a,a,a)
type Tup8 a = (a,a,a,a,a,a,a,a)
type Tup9 a = (a,a,a,a,a,a,a,a,a)

refListMul
    :: Semiring a
    => [a] -> [a] -> [a]
refListMul [] _              = []
refListMul _ []              = []
refListMul (x:xs) yys@(y:ys) = (x <.> y) : map (x <.>) ys <+> xs <.> yys

typeclassTests :: TestTree
typeclassTests =
    testGroup
        "typeclass tests"
        [ let p = Proxy :: Proxy (PositiveInfinite Int)
          in testGroup "PositiveInfinite" [storableQC p, liftedQC p]
        , let p = Proxy :: Proxy (NegativeInfinite Int)
          in testGroup "NegativeInfinite" [storableQC p, liftedQC p]
        , let p = Proxy :: Proxy (Infinite Int)
          in testGroup "Infinite" [storableQC p, liftedQC p]
        , let p = Proxy :: Proxy (Add A)
          in testGroup "Add" [liftedQC p]
        , let p = Proxy :: Proxy (Mul A)
          in testGroup "Mul" [liftedQC p]
        , let p = Proxy :: Proxy (Max A)
          in testGroup "Max" [liftedQC p]
        , let p = Proxy :: Proxy (Min A)
          in testGroup "Min" [liftedQC p]
        , let p = Proxy :: Proxy (Bottleneck A)
          in testGroup "Min" [liftedQC p]
        , let p = Proxy :: Proxy (Division A)
          in testGroup "Min" [liftedQC p]
        , let p = Proxy :: Proxy (Łukasiewicz A)
          in testGroup "Min" [liftedQC p]
        , let p = Proxy :: Proxy (Viterbi A)
          in testGroup "Min" [liftedQC p]
        , let p = Proxy :: Proxy (PosFrac A)
          in testGroup "Min" [liftedQC p]
        , let p = Proxy :: Proxy (PosInt A)
          in testGroup "Min" [liftedQC p]
        ]

semiringLawTests :: TestTree
semiringLawTests =
    testGroup
        "Semiring/StarSemiring Laws"
        [ let p = Proxy :: Proxy (ApproxLog Double)
          in testGroup "Log" [semiringLawsSC p]
        , let p = Proxy :: Proxy (SApproxLog Double)
          in testGroup "Log" [semiringLawsSC p]
        , let p = Proxy :: Proxy (Map String Int)
          in testGroup
                 "Map"
                 [localOption (QC.QuickCheckMaxSize 10) $ semiringLawsQC p]
        , let p0 = Proxy :: Proxy (Matrix V0 V0 Integer)
              p1 = Proxy :: Proxy (Matrix V1 V1 Integer)
              p2 = Proxy :: Proxy (Matrix V2 V2 Integer)
              p5 = Proxy :: Proxy (Matrix V5 V5 Integer)
          in testGroup
                 "Matrix"
                 [ testGroup "0" [semiringLawsQC p0]
                 , testGroup "1" [semiringLawsQC p1]
                 , testGroup "2" [semiringLawsQC p2]
                 , testGroup "5" [semiringLawsQC p5]]
        , let p = Proxy :: Proxy Integer
          in testGroup
                 "Integer"
                 [semiringLawsSC p, ordLawsSC p, zeroLawsSC p, ordLawsQC p]
        , let p = Proxy :: Proxy (Func Bool Bool)
          in testGroup "Bool -> Bool" [semiringLawsQC p]
        , testGroup
              "Endo Bool"
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
          in testGroup "WordOfSize 2" [semiringLawsSC p, zeroLawsSC p]
        , let p = Proxy :: Proxy (Tup2 (WordOfSize 2))
          in testGroup "Tup2 (WordOfSize 2)" [semiringLawsSC p, zeroLawsSC p]
        , let p = Proxy :: Proxy (Tup3 (WordOfSize 2))
          in testGroup "Tup3 (WordOfSize 2)" [semiringLawsQC p, zeroLawsQC p]
        , let p = Proxy :: Proxy (Tup4 Int)
          in testGroup "Tup4 Int" [semiringLawsQC p, zeroLawsQC p]
        , let p = Proxy :: Proxy (Tup5 Int)
          in testGroup "Tup5 Int" [semiringLawsQC p, zeroLawsQC p]
        , let p = Proxy :: Proxy (Tup6 Int)
          in testGroup "Tup6 Int" [semiringLawsQC p, zeroLawsQC p]
        , let p = Proxy :: Proxy (Tup7 Int)
          in testGroup "Tup7 Int" [semiringLawsQC p, zeroLawsQC p]
        , let p = Proxy :: Proxy (Tup8 Int)
          in testGroup "Tup8 Int" [semiringLawsQC p, zeroLawsQC p]
        , let p = Proxy :: Proxy (Tup9 Int)
          in testGroup "Tup9 Int" [semiringLawsQC p, zeroLawsQC p]
        , let p = Proxy :: Proxy (Tup2 (PositiveInfinite (WordOfSize 2)))
          in testGroup "Tup2 (WordOfSize 2)" [starLawsSC p]
        , let p = Proxy :: Proxy (Tup3 (PositiveInfinite (WordOfSize 2)))
          in testGroup "Tup3 (WordOfSize 2)" [starLawsSC p]
        , let p = Proxy :: Proxy (Tup4 (PositiveInfinite Int))
          in testGroup "Tup4 Int" [starLawsQC p]
        , let p = Proxy :: Proxy (Tup5 (PositiveInfinite Int))
          in testGroup "Tup5 Int" [starLawsQC p]
        , let p = Proxy :: Proxy (Tup6 (PositiveInfinite Int))
          in testGroup "Tup6 Int" [starLawsQC p]
        , let p = Proxy :: Proxy (Tup7 (PositiveInfinite Int))
          in testGroup "Tup7 Int" [starLawsQC p]
        , let p = Proxy :: Proxy (Tup8 (PositiveInfinite Int))
          in testGroup "Tup8 Int" [starLawsQC p]
        , let p = Proxy :: Proxy (Tup9 (PositiveInfinite Int))
          in testGroup "Tup9 Int" [starLawsQC p]
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
              , SC.testProperty "mulId" (mulId :: UnaryLaws (Infinite Integer))
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
        , let p = Proxy :: Proxy [Integer]
          in testGroup
                 "[Integer]"
                 [ semiringLawsQC p
                 , starLawsQC
                       (Proxy :: Proxy (LimitSize 100 (PositiveInfinite Integer)))
                 , QC.testProperty
                       "reference implementation of <.>"
                       (\xs ys ->
                             (xs <.> ys) ===
                             refListMul xs (ys :: [WordOfSize 2]))]
        , let p = Proxy :: Proxy (Vector.Vector Int)
          in testGroup
                 "Vector Int"
                 [ semiringLawsQC p
                 , QC.testProperty
                       "reference implementation of <.>"
                       (\xs ys ->
                             (xs <.> ys :: [Int]) ===
                             Vector.toList
                                 (Vector.fromList xs <.> Vector.fromList ys))]
        , let p = Proxy :: Proxy (Storable.Vector Int)
          in testGroup
                 "Storable Vector Int"
                 [ semiringLawsQC p
                 , QC.testProperty
                       "reference implementation of <.>"
                       (\xs ys ->
                             (xs <.> ys :: [Int]) ===
                             Vector.toList
                                 (Vector.fromList xs <.> Vector.fromList ys))]
        , let p = Proxy :: Proxy (Unboxed.Vector Int)
          in testGroup
                 "Unboxed Vector Int"
                 [ semiringLawsQC p
                 , QC.testProperty
                       "reference implementation of <.>"
                       (\xs ys ->
                             (xs <.> ys :: [Int]) ===
                             Unboxed.toList
                                 (Unboxed.fromList xs <.> Unboxed.fromList ys))]
        , testGroup
              "Unboxed Vector (NegativeInfinite Int)"
              [ QC.testProperty
                    "reference implementation of <.>"
                    (\xs ys ->
                          (xs <.> ys :: [NegativeInfinite Int]) ===
                          Unboxed.toList
                              (Unboxed.fromList xs <.> Unboxed.fromList ys))]
        , testGroup
              "Unboxed Vector (Infinite Int)"
              [ QC.testProperty
                    "reference implementation of <.>"
                    (\xs ys ->
                          (xs <.> ys :: [Infinite Int]) ===
                          Unboxed.toList
                              (Unboxed.fromList xs <.> Unboxed.fromList ys))]
        , let p = Proxy :: Proxy (Min (PositiveInfinite Integer))
          in testGroup "Min Inf Integer" [semiringLawsSC p, zeroLawsSC p]
        , let p = Proxy :: Proxy (Min (Infinite Integer))
          in testGroup "Min Inf Integer" [starLawsSC p]
        , let p = Proxy :: Proxy (Max (NegativeInfinite Integer))
          in testGroup "Max NegInf Integer" [semiringLawsSC p, zeroLawsSC p]
        , let p = Proxy :: Proxy (Max (Infinite Integer))
          in testGroup "Max Inf Integer" [starLawsSC p]
        , let p = Proxy :: Proxy (Free (WordOfSize 2))
          in testGroup
                 "Free (WordOfSize 2)"
                 [localOption (QC.QuickCheckMaxSize 10) $ semiringLawsQC p]
        , let p = Proxy :: Proxy (Division Integer)
          in testGroup "Division Integer" [semiringLawsSC p, zeroLawsSC p]
        , let p = Proxy :: Proxy (Łukasiewicz Fraction)
          in testGroup "Łukasiewicz Fraction" [semiringLawsSC p, zeroLawsSC p]
        , let p = Proxy :: Proxy (Viterbi Fraction)
          in testGroup "Viterbi Fraction" [semiringLawsSC p, zeroLawsSC p]]

main :: IO ()
main = do
    doctest ["-isrc", "src/"]
    defaultMain $ testGroup "Tests" [typeclassTests, semiringLawTests]
