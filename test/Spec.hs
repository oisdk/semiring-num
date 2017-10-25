{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main (main) where

import           Data.Proxy
import           Data.Typeable
import           Data.Monoid

import           Data.Map.Strict          (Map)

import qualified Data.Vector              as Vector
import qualified Data.Vector.Storable     as Storable
import qualified Data.Vector.Unboxed      as Unboxed

import           Data.Semiring
import           Data.Semiring.Infinite
import           Data.Semiring.Numeric

import           Numeric.Natural
import           Numeric.Sized.WordOfSize

import           Test.DocTest
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
import           Properties

type Tup2 a = (a,a)
type Tup3 a = (a,a,a)
type Tup4 a = (a,a,a,a)
type Tup5 a = (a,a,a,a,a)
type Tup6 a = (a,a,a,a,a,a)
type Tup7 a = (a,a,a,a,a,a,a)
type Tup8 a = (a,a,a,a,a,a,a,a)
type Tup9 a = (a,a,a,a,a,a,a,a,a)

groupType :: Typeable a => Proxy a -> [Proxy a -> TestTree] -> TestTree
groupType p = testGroup (show (typeRep p)) . map ($p)

typeclassTests :: TestTree
typeclassTests =
    testGroup
        "typeclass tests"
        [ storableQCT (Proxy :: Proxy
              [ PositiveInfinite Int
              , NegativeInfinite Int
              , Infinite Int
              ])
        , liftedQCT (Proxy :: Proxy
              [ PositiveInfinite
              , NegativeInfinite
              , Infinite
              , Add
              , Mul
              , Max
              , Min
              , Bottleneck
              , Division
              , Łukasiewicz
              , Viterbi
              , PosFrac
              , PosInt
              ])]

semiringLawTests :: TestTree
semiringLawTests =
    testGroup
        "Semiring/StarSemiring Laws"
        [ semiringLawsSCT (Proxy :: Proxy
              [ ApproxLog Double
              , SApproxLog Double
              , Integer
              , PositiveInfinite Natural
              , Int
              , WordOfSize 2
              , Tup2 (WordOfSize 2)
              , ()
              , Bool
              , Any
              , All
              , Min (PositiveInfinite Integer)
              , Max (NegativeInfinite Integer)
              , Division Integer
              , Viterbi Fraction
              , Łukasiewicz Fraction
              ])
        , semiringLawsQCT (Proxy :: Proxy
              [ Matrix V0 V0 Integer
              , Matrix V1 V1 Integer
              , Matrix V2 V2 Integer
              , Matrix V5 V5 Integer
              , Func Bool Bool
              , Tup3 (WordOfSize 2)
              , Tup4 Int
              , Tup5 Int
              , Tup6 Int
              , Tup7 Int
              , Tup8 Int
              , Tup9 Int
              , [Int]
              , Vector.Vector Int
              , Storable.Vector Int
              , Unboxed.Vector Int
              ])
        , starLawsQCT (Proxy :: Proxy
              [ Tup4 (PositiveInfinite Int)
              , Tup5 (PositiveInfinite Int)
              , Tup6 (PositiveInfinite Int)
              , Tup7 (PositiveInfinite Int)
              , Tup8 (PositiveInfinite Int)
              , Tup9 (PositiveInfinite Int)
              , LimitSize 100 (PositiveInfinite Integer)
              ])
        , starLawsSCT (Proxy :: Proxy
              [ Tup2 (PositiveInfinite (WordOfSize 2))
              , Tup3 (PositiveInfinite (WordOfSize 2))
              , ()
              , Bool
              , Any
              , All
              , Min (Infinite Integer)
              , Max (Infinite Integer)
              ])
        , ordLawsQCT (Proxy :: Proxy '[])
        , ordLawsSCT (Proxy :: Proxy
              [ Integer
              , PositiveInfinite Natural
              , Int
              , ()
              , Bool
              , Any
              , All
              ])
        , zeroLawsQCT (Proxy :: Proxy
              [ Tup3 (WordOfSize 2)
              , Tup4 Int
              , Tup5 Int
              , Tup6 Int
              , Tup7 Int
              , Tup8 Int
              , Tup9 Int
              ])
        , zeroLawsSCT (Proxy :: Proxy
              [ Integer
              , PositiveInfinite Natural
              , Int
              , WordOfSize 2
              , Tup2 (WordOfSize 2)
              , ()
              , Bool
              , Any
              , All
              , Min (PositiveInfinite Integer)
              , Max (NegativeInfinite Integer)
              , Division Integer
              , Viterbi Fraction
              , Łukasiewicz Fraction
              ])
        , refListMulQCT (Proxy :: Proxy
              [ [Int]
              , Vector.Vector Int
              , Unboxed.Vector Int
              , Storable.Vector Int
              , Unboxed.Vector (NegativeInfinite Int)
              , Unboxed.Vector (Infinite Int)
              ])
        , groupType
              (Proxy :: Proxy (Map String Int))
              [localOption (QC.QuickCheckMaxSize 10) . semiringLawsQC]
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
        ]

main :: IO ()
main = do
    doctest ["-isrc", "src/"]
    defaultMain $ testGroup "Tests" [typeclassTests, semiringLawTests]
