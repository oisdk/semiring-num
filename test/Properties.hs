{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE RankNTypes           #-}

module Properties where

import           Data.Semiring
import           Test.Semiring

import           Test.QuickCheck          hiding (Positive (..), generate,
                                           (.&.))
import           Test.SmallCheck.Series hiding (Positive)
import           Test.Tasty
import qualified Test.Tasty.QuickCheck  as QC
import qualified Test.Tasty.SmallCheck  as SC
import           Test.QuickCheck.Poly

import qualified Data.Vector.Storable     as Storable
import           Data.Proxy
import           Data.Typeable
import           Data.Functor.Classes
import           GHC.Exts (IsList(..))

import           TypeLevel
import           Orphans ()
import           CompUtils

--------------------------------------------------------------------------------
-- Property lists, generic to the test framework
--------------------------------------------------------------------------------

semiringLawsCL
    :: (Show r
       ,Eq r
       ,Semiring r
       ,testable (r -> Either String String)
       ,testable (r -> r -> Either String String)
       ,testable (r -> r -> r -> Either String String))
    => Proxy testable
    -> (forall f. testable f =>
                  String -> f -> TestTree)
    -> Proxy r
    -> [TestTree]
semiringLawsCL _ f (_ :: Proxy r) =
  [ f "plusId" (plusId :: r -> Either String String)
  , f "mulId" (mulId  :: r -> Either String String)
  , f "annihilateL" (annihilateL  :: r -> Either String String)
  , f "annihilateR" (annihilateR  :: r -> Either String String)
  , f "plusComm" (plusComm  :: r -> r -> Either String String)
  , f "plusAssoc" (plusAssoc  :: r -> r -> r -> Either String String)
  , f "mulAssoc" (mulAssoc  :: r -> r -> r -> Either String String)
  , f "mulDistribL" (mulDistribL  :: r -> r -> r -> Either String String)
  , f "mulDistribR" (mulDistribR  :: r -> r -> r -> Either String String)]

starLawsCL
    :: (Show r, Eq r, StarSemiring r, testable (r -> Either String String))
    => Proxy testable
    -> (forall f. testable f =>
                  String -> f -> TestTree)
    -> Proxy r
    -> [TestTree]
starLawsCL _ f (_ :: Proxy r) =
  [ f "starLaw" (starLaw :: r -> Either String String)
  , f "plusLaw" (plusLaw :: r -> Either String String)]

ordLawsCL
    :: (Show r, Ord r, Semiring r, testable (r -> r -> r -> Either String String))
    => Proxy testable
    -> (forall f. testable f =>
                  String -> f -> TestTree)
    -> Proxy r
    -> [TestTree]
ordLawsCL _ f (_ :: Proxy r) =
  [ f "mulLaw" (ordMulLaw :: r -> r -> r -> Either String String)
  , f "addLaw" (ordAddLaw :: r -> r -> r -> Either String String)]

--------------------------------------------------------------------------------
-- Property lists: SmallCheck
--------------------------------------------------------------------------------

semiringLawsSCL :: (Show r, Eq r, Semiring r, Serial IO r) => f r -> [TestTree]
semiringLawsSCL (_ :: f r) =
    semiringLawsCL
        (Proxy :: Proxy (SC.Testable IO))
        SC.testProperty
        (Proxy :: Proxy r)

starLawsSCL
    :: (Show r, Eq r, StarSemiring r, Serial IO r)
    => f r -> [TestTree]
starLawsSCL (_ :: f r) =
    starLawsCL
        (Proxy :: Proxy (SC.Testable IO))
        SC.testProperty
        (Proxy :: Proxy r)

zeroLawsSCL
    :: (Show r, Eq r, DetectableZero r, Serial IO r)
    => f r -> [TestTree]
zeroLawsSCL (_ :: f r) =
    [ SC.testProperty "zeroLaw" (zeroLaw :: r -> Either String String)
    , SC.testProperty "zeroIsZero" (zeroIsZero (Proxy :: Proxy r))]

ordLawsSCL
    :: (Show r, Ord r, Semiring r, Serial IO r)
    => f r -> [TestTree]
ordLawsSCL (_ :: f r) =
    ordLawsCL
        (Proxy :: Proxy (SC.Testable IO))
        SC.testProperty
        (Proxy :: Proxy r)

--------------------------------------------------------------------------------
-- Property lists: QuickCheck
--------------------------------------------------------------------------------

semiringLawsQCL :: (Show r, Eq r, Semiring r, Arbitrary r) => f r -> [TestTree]
semiringLawsQCL (_ :: f r) =
    semiringLawsCL
        (Proxy :: Proxy QC.Testable)
        QC.testProperty
        (Proxy :: Proxy r)

starLawsQCL
    :: (Show r, Eq r, StarSemiring r, Arbitrary r)
    => f r -> [TestTree]
starLawsQCL (_ :: f r) =
    starLawsCL (Proxy :: Proxy QC.Testable) QC.testProperty (Proxy :: Proxy r)

zeroLawsQCL
    :: (Show r, Eq r, DetectableZero r, Arbitrary r)
    => f r -> [TestTree]
zeroLawsQCL (_ :: f r) =
    [ QC.testProperty "zeroLaw" (zeroLaw :: r -> Either String String)
    , QC.testProperty "zeroIsZero" (once $ zeroIsZero (Proxy :: Proxy r))]

ordLawsQCL
    :: (Show r, Ord r, Semiring r, Arbitrary r)
    => f r -> [TestTree]
ordLawsQCL (_ :: f r) =
    ordLawsCL
        (Proxy :: Proxy QC.Testable)
        QC.testProperty
        (Proxy :: Proxy r)

storableQCL
    :: (Show r, Eq r, Arbitrary r, Storable.Storable r)
    => f r -> [TestTree]
storableQCL (_ :: f r) =
        [ QC.testProperty
              "unstore . store == id"
              (\(xs :: [r]) ->
                    (Storable.toList |.| Storable.fromList) xs === xs)]

liftedQCL
    :: (Show1 r
       ,Eq1 r
       ,Ord1 r
       ,Read1 r
       ,Arbitrary (r A)
       ,Show (r A)
       ,Eq (r A)
       ,Ord (r A)
       ,Read A)
    => f r -> [TestTree]
liftedQCL (_ :: f r) =
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

--------------------------------------------------------------------------------
-- Single-type tests: SmallCheck
--------------------------------------------------------------------------------

ordLawsSC :: (Show r, Ord r, Semiring r, Serial IO r) => f r -> TestTree
ordLawsSC (p :: f r) = testGroup "Ordering laws" (ordLawsSCL p)

zeroLawsSC :: (Show r, Eq r, DetectableZero r, Serial IO r) => f r -> TestTree
zeroLawsSC (p :: f r) = testGroup "Zero laws" (zeroLawsSCL p)

--------------------------------------------------------------------------------
-- Single-type tests: QuickCheck
--------------------------------------------------------------------------------

semiringLawsQC
    :: (Show r, Eq r, Semiring r, Arbitrary r)
    => f r -> TestTree
semiringLawsQC (p :: f r) = testGroup "Semiring Laws" (semiringLawsQCL p)

zeroLawsQC :: (Show r, Eq r, DetectableZero r, Arbitrary r) => f r -> TestTree
zeroLawsQC (p :: f r) = testGroup "Zero laws" (zeroLawsQCL p)

refListMulQC
    :: (Semiring (Item t), Semiring t, Eq (Item t), Show (Item t), IsList t)
    => Proxy t -> [Item t] -> [Item t] -> Property
refListMulQC (_ :: Proxy t) xs ys =
    refListMul xs ys ===
    (toList :: t -> [Item t]) (fromList xs <.> fromList ys)
  where
    refListMul [] _ = []
    refListMul _ [] = []
    refListMul (x:xs') yys@(y:ys') = (x <.> y) : map (x <.>) ys' <+> xs' <.> yys

--------------------------------------------------------------------------------
-- Multi-type tests: SmallCheck
--------------------------------------------------------------------------------

semiringLawsSCT
    :: (Reifiable xs
       ,AllAre (Diffable & Semiring & Serial IO) xs)
    => Proxy xs -> TestTree
semiringLawsSCT p =
    testGroup
        "Semiring Laws"
        (reify
             (Proxy :: Proxy (Diffable & Semiring & Serial IO))
             p
             (nameType semiringLawsSCL))

starLawsSCT
    :: (Reifiable xs
       ,AllAre (Diffable & StarSemiring & Serial IO) xs)
    => Proxy xs -> TestTree
starLawsSCT p =
    testGroup
        "Star Laws"
        (reify
             (Proxy :: Proxy (Diffable & StarSemiring & Serial IO))
             p
             (nameType starLawsSCL))

ordLawsSCT
    :: (Reifiable xs
       ,AllAre (Diffable & Serial IO & Semiring & Ord) xs)
    => Proxy xs -> TestTree
ordLawsSCT p =
    testGroup
        "Ordering laws"
        (reify
             (Proxy :: Proxy (Diffable & Serial IO & Semiring & Ord))
             p
             (nameType ordLawsSCL))

zeroLawsSCT
    :: (Reifiable xs
       ,AllAre (Diffable & Serial IO & DetectableZero) xs)
    => Proxy xs -> TestTree
zeroLawsSCT p =
    testGroup
        "Zero laws"
        (reify
             (Proxy :: Proxy (Diffable & Serial IO & DetectableZero))
             p
             (nameType zeroLawsSCL))

--------------------------------------------------------------------------------
-- Multi-type tests: QuickCheck
--------------------------------------------------------------------------------

starLawsQCT
    :: (Reifiable xs
       ,AllAre (Diffable & StarSemiring & Arbitrary) xs)
    => Proxy xs -> TestTree
starLawsQCT p =
    testGroup
        "Star Laws"
        (reify
             (Proxy :: Proxy (Diffable & StarSemiring & Arbitrary))
             p
             (nameType starLawsQCL))

semiringLawsQCT
    :: (Reifiable xs
       ,AllAre (Show & Eq & Semiring & Arbitrary & Typeable) xs)
    => Proxy xs -> TestTree
semiringLawsQCT p =
    testGroup
        "Semiring Laws"
        (reify
             (Proxy :: Proxy (Show & Eq & Semiring & Arbitrary & Typeable))
             p
             (nameType semiringLawsQCL))

ordLawsQCT
    :: (Reifiable xs
       ,AllAre (Diffable & Arbitrary & Semiring & Ord) xs)
    => Proxy xs -> TestTree
ordLawsQCT p =
    testGroup
        "Ordering laws"
        (reify
             (Proxy :: Proxy (Diffable & Arbitrary & Semiring & Ord))
             p
             (nameType ordLawsQCL))

zeroLawsQCT
    :: (Reifiable xs
       ,AllAre (Diffable & Arbitrary & DetectableZero) xs)
    => Proxy xs -> TestTree
zeroLawsQCT p =
    testGroup
        "Zero laws"
        (reify
             (Proxy :: Proxy (Diffable & Arbitrary & DetectableZero))
             p
             (nameType zeroLawsQCL))

storableQCT
    :: (Reifiable xs
       ,AllAre (Diffable & Arbitrary & Storable.Storable) xs)
    => Proxy xs -> TestTree
storableQCT p =
    testGroup
        "Storable implementation"
        (reify
             (Proxy :: Proxy (Diffable & Arbitrary & Storable.Storable))
             p
             (nameType storableQCL))

class (IsList t
      ,Semiring (Item t)
      ,Arbitrary (Item t)
      ,Show (Item t)
      ,Eq (Item t)
      ,Typeable t
      ,Semiring t) =>
      ListPoly t
instance (IsList t
         ,Semiring (Item t)
         ,Arbitrary (Item t)
         ,Show (Item t)
         ,Eq (Item t)
         ,Typeable t
         ,Semiring t) =>
         ListPoly t

refListMulQCT
    :: (Reifiable xs, AllAre ListPoly xs)
    => Proxy xs -> TestTree
refListMulQCT p =
    testGroup
        "Reference convolution"
        (reify
             (Proxy :: Proxy ListPoly)
             p
             (\(t :: Proxy l) ->
                   QC.testProperty (show (typeRep t)) (refListMulQC t)))

class (Show1 r
      ,Eq1 r
      ,Ord1 r
      ,Read1 r
      ,Arbitrary (r A)
      ,Show (r A)
      ,Eq (r A)
      ,Ord (r A)
      ,Typeable (r A)) =>
      Lifted r
instance (Show1 r
         ,Eq1 r
         ,Ord1 r
         ,Read1 r
         ,Arbitrary (r A)
         ,Show (r A)
         ,Eq (r A)
         ,Ord (r A)
         ,Typeable (r A)) =>
         Lifted r

liftedQCT
    :: (Reifiable xs, AllAre Lifted xs)
    => Proxy xs -> TestTree
liftedQCT p =
    testGroup
        "Lifted Classes"
        (reify
             (Proxy :: Proxy Lifted)
             p
             (\(t :: Proxy f) ->
                   testGroup
                       (show (typeRep (Proxy :: Proxy (f A))))
                       (liftedQCL t)))

--------------------------------------------------------------------------------
-- Utils
--------------------------------------------------------------------------------

nameType :: Typeable a => (Proxy a -> [TestTree]) -> Proxy a -> TestTree
nameType f p = testGroup (show (typeRep p)) (f p)

type Diffable = Show & Eq & Typeable

(=.=) :: (Eq a, Show a) => [a] -> [a] -> Property
xs =.= ys = go xs ys where
  go [] [] = property True
  go [] ys' = counterexample (show xs ++ "\n/=\n" ++ show ys ++ "\nrhs longer, with extra elements:\n" ++ show ys') False
  go xs' [] = counterexample (show xs ++ "\n/=\n" ++ show ys ++ "\nlhs longer, with extra elements:\n" ++ show xs') False
  go (x:xs') (y:ys')
      | x /= y = counterexample (show xs ++ "\n/=\n" ++ show ys ++ "\n" ++ show x ++ " /= " ++ show y) False
      | otherwise = go xs' ys'
