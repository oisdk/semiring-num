{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

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

nameType :: Typeable a => (Proxy a -> [TestTree]) -> Proxy a -> TestTree
nameType f p = testGroup (show (typeRep p)) (f p)

semiringLawsSCL :: (Show r, Eq r, Semiring r, Serial IO r) => f r -> [TestTree]
semiringLawsSCL (_ :: f r) =
  [ SC.testProperty "plusId" (plusId :: r -> Either String String)
  , SC.testProperty "mulId" (mulId  :: r -> Either String String)
  , SC.testProperty "annihilateL" (annihilateL  :: r -> Either String String)
  , SC.testProperty "annihilateR" (annihilateR  :: r -> Either String String)
  , SC.testProperty "plusComm" (plusComm  :: r -> r -> Either String String)
  , SC.testProperty "plusAssoc" (plusAssoc  :: r -> r -> r -> Either String String)
  , SC.testProperty "mulAssoc" (mulAssoc  :: r -> r -> r -> Either String String)
  , SC.testProperty "mulDistribL" (mulDistribL  :: r -> r -> r -> Either String String)
  , SC.testProperty "mulDistribR" (mulDistribR  :: r -> r -> r -> Either String String)]

semiringLawsQCL :: (Show r, Eq r, Semiring r, Arbitrary r) => f r -> [TestTree]
semiringLawsQCL (_ :: f r) =
  [ QC.testProperty "plusId" (plusId :: r -> Either String String)
  , QC.testProperty "mulId" (mulId  :: r -> Either String String)
  , QC.testProperty "annihilateL" (annihilateL  :: r -> Either String String)
  , QC.testProperty "annihilateR" (annihilateR  :: r -> Either String String)
  , QC.testProperty "plusComm" (plusComm  :: r -> r -> Either String String)
  , QC.testProperty "plusAssoc" (plusAssoc  :: r -> r -> r -> Either String String)
  , QC.testProperty "mulAssoc" (mulAssoc  :: r -> r -> r -> Either String String)
  , QC.testProperty "mulDistribL" (mulDistribL  :: r -> r -> r -> Either String String)
  , QC.testProperty "mulDistribR" (mulDistribR  :: r -> r -> r -> Either String String)]

starLawsQCL :: (Show r, Eq r, StarSemiring r, Arbitrary r) => f r -> [TestTree]
starLawsQCL (_ :: f r) =
  [ QC.testProperty "starLaw" (starLaw :: r -> Either String String)
  , QC.testProperty "plusLaw" (plusLaw :: r -> Either String String)]

semiringLawsSC :: (Show r, Eq r, Semiring r, Serial IO r) => f r -> TestTree
semiringLawsSC (p :: f r) = testGroup "Semiring Laws" (semiringLawsSCL p)

semiringLawsQC :: (Show r, Eq r, Semiring r, Arbitrary r) => f r -> TestTree
semiringLawsQC (p :: f r) = testGroup "Semiring Laws" (semiringLawsQCL p)

starLawsQC :: (Show r, Eq r, StarSemiring r, Arbitrary r) => f r -> TestTree
starLawsQC (p :: f r) = testGroup "Star laws" (starLawsQCL p)

starLawsQCT
    :: (Reifiable xs
       ,AllAre (Show && Eq && StarSemiring && Arbitrary && Typeable) xs)
    => Proxy xs -> TestTree
starLawsQCT p =
    testGroup
        "Star Laws"
        (reify
             (Proxy :: Proxy (Show && Eq && StarSemiring && Arbitrary && Typeable))
             p
             (nameType starLawsQCL))

starLawsSCT
    :: (Reifiable xs
       ,AllAre (Show && Eq && StarSemiring && Serial IO && Typeable) xs)
    => Proxy xs -> TestTree
starLawsSCT p =
    testGroup
        "Star Laws"
        (reify
             (Proxy :: Proxy (Show && Eq && StarSemiring && Serial IO && Typeable))
             p
             (nameType starLawsSCL))

semiringLawsQCT
    :: (Reifiable xs
       ,AllAre (Show && Eq && Semiring && Arbitrary && Typeable) xs)
    => Proxy xs -> TestTree
semiringLawsQCT p =
    testGroup
        "Semiring Laws"
        (reify
             (Proxy :: Proxy (Show && Eq && Semiring && Arbitrary && Typeable))
             p
             (nameType semiringLawsQCL))

semiringLawsSCT
    :: (Reifiable xs
       ,AllAre (Show && Eq && Semiring && Serial IO && Typeable) xs)
    => Proxy xs -> TestTree
semiringLawsSCT p =
    testGroup
        "Semiring Laws"
        (reify
             (Proxy :: Proxy (Show && Eq && Semiring && Serial IO && Typeable))
             p
             (nameType semiringLawsSCL))

starLawsSC :: (Show r, Eq r, StarSemiring r, Serial IO r) => f r -> TestTree
starLawsSC (p :: f r) = testGroup "Star laws" (starLawsSCL p)

starLawsSCL :: (Show r, Eq r, StarSemiring r, Serial IO r) => f r -> [TestTree]
starLawsSCL (_ :: f r) =
  [ SC.testProperty "starLaw" (starLaw :: r -> Either String String)
  , SC.testProperty "plusLaw" (plusLaw :: r -> Either String String)]

ordLawsQCL :: (Show r, Ord r, Semiring r, Arbitrary r) => f r -> [TestTree]
ordLawsQCL (_ :: f r) =
  [ QC.testProperty "mulLaw" (ordMulLaw :: r -> r -> r -> Either String String)
  , QC.testProperty "addLaw" (ordAddLaw :: r -> r -> r -> Either String String)]

zeroLawsQCL :: (Show r, Eq r, DetectableZero r, Arbitrary r) => f r -> [TestTree]
zeroLawsQCL (_ :: f r) =
  [ QC.testProperty "zeroLaw" (zeroLaw :: r -> Either String String)
  , QC.testProperty "zeroIsZero" (once $ zeroIsZero (Proxy :: Proxy r))]

ordLawsSCL :: (Show r, Ord r, Semiring r, Serial IO r) => f r -> [TestTree]
ordLawsSCL (_ :: f r) =
  [ SC.testProperty "mulLaw" (ordMulLaw :: r -> r -> r -> Either String String)
  , SC.testProperty "addLaw" (ordAddLaw :: r -> r -> r -> Either String String)]

zeroLawsSCL :: (Show r, Eq r, DetectableZero r, Serial IO r) => f r -> [TestTree]
zeroLawsSCL (_ :: f r) =
  [ SC.testProperty "zeroLaw" (zeroLaw :: r -> Either String String)
  , SC.testProperty "zeroIsZero" (zeroIsZero (Proxy :: Proxy r))]

ordLawsQC :: (Show r, Ord r, Semiring r, Arbitrary r) => f r -> TestTree
ordLawsQC (p :: f r) = testGroup "Ordering laws" (ordLawsQCL p)

zeroLawsQC :: (Show r, Eq r, DetectableZero r, Arbitrary r) => f r -> TestTree
zeroLawsQC (p :: f r) = testGroup "Zero laws" (zeroLawsQCL p)

ordLawsSC :: (Show r, Ord r, Semiring r, Serial IO r) => f r -> TestTree
ordLawsSC (p :: f r) = testGroup "Ordering laws" (ordLawsSCL p)

zeroLawsSC :: (Show r, Eq r, DetectableZero r, Serial IO r) => f r -> TestTree
zeroLawsSC (p :: f r) = testGroup "Zero laws" (zeroLawsSCL p)

storableQC :: (Show r, Eq r, Arbitrary r, Storable.Storable r) => f r -> TestTree
storableQC (p :: f r) = testGroup "Storable implementation" (storableQCL p)

ordLawsQCT
    :: (Reifiable xs
       ,AllAre (Show && Eq && Arbitrary && Typeable && Semiring && Ord) xs)
    => Proxy xs -> TestTree
ordLawsQCT p =
    testGroup
        "Ordering laws"
        (reify
             (Proxy :: Proxy (Show && Eq && Arbitrary && Typeable && Semiring && Ord))
             p
             (nameType ordLawsQCL))

ordLawsSCT
    :: (Reifiable xs
       ,AllAre (Show && Eq && Serial IO && Typeable && Semiring && Ord) xs)
    => Proxy xs -> TestTree
ordLawsSCT p =
    testGroup
        "Ordering laws"
        (reify
             (Proxy :: Proxy (Show && Eq && Serial IO && Typeable && Semiring && Ord))
             p
             (nameType ordLawsSCL))

zeroLawsSCT :: (Reifiable xs, AllAre (Show && Eq && Serial IO && Typeable && DetectableZero) xs) => Proxy xs -> TestTree
zeroLawsSCT p =
    testGroup
        "Zero laws"
        (reify
             (Proxy :: Proxy (Show && Eq && Serial IO && Typeable && DetectableZero))
             p
             (nameType zeroLawsSCL))

zeroLawsQCT :: (Reifiable xs, AllAre (Show && Eq && Arbitrary && Typeable && DetectableZero) xs) => Proxy xs -> TestTree
zeroLawsQCT p =
    testGroup
        "Zero laws"
        (reify
             (Proxy :: Proxy (Show && Eq && Arbitrary && Typeable && DetectableZero))
             p
             (nameType zeroLawsQCL))

storableQCL :: (Show r, Eq r, Arbitrary r, Storable.Storable r) => f r -> [TestTree]
storableQCL (_ :: f r) =
        [ QC.testProperty
              "unstore . store == id"
              (\(xs :: [r]) ->
                    (Storable.toList |.| Storable.fromList) xs === xs)]

storableQCT
    :: (Reifiable xs
       ,AllAre (Show && Eq && Arbitrary && Typeable && Storable.Storable) xs)
    => Proxy xs -> TestTree
storableQCT p =
    testGroup
        "Storable implementation"
        (reify
             (Proxy :: Proxy (Show && Eq && Arbitrary && Typeable && Storable.Storable))
             p
             (nameType storableQCL))


class (IsList t, Semiring (Item t), Arbitrary (Item t), Show (Item t), Eq (Item t)) => ListPoly t
instance (IsList t, Semiring (Item t), Arbitrary (Item t), Show (Item t), Eq (Item t)) => ListPoly t


refListMulQCT :: (Reifiable xs, AllAre (Typeable && Semiring && ListPoly) xs) => Proxy xs -> TestTree
refListMulQCT p =
    testGroup
        "Reference convolution"
        (reify
             (Proxy :: Proxy (Typeable && Semiring && ListPoly))
             p
             (\(t :: Proxy l) ->
                   QC.testProperty
                       (show (typeRep t))
                       (refListMulQC t)
                            ))


infixr 9 |.|
(|.|) :: (b -> c) -> (a -> b) -> a -> c
(|.|) f g x = f (g x)
{-# NOINLINE (|.|) #-}


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

refListMul
    :: Semiring a
    => [a] -> [a] -> [a]
refListMul [] _              = []
refListMul _ []              = []
refListMul (x:xs) yys@(y:ys) = (x <.> y) : map (x <.>) ys <+> xs <.> yys

refListMulQC
    :: (Semiring (Item t), Semiring t, Eq (Item t), Show (Item t), IsList t)
    => Proxy t -> [Item t] -> [Item t] -> Property
refListMulQC (_ :: Proxy t) xs ys =
    refListMul xs ys ===
    (toList :: t -> [Item t]) (fromList xs <.> fromList ys)
