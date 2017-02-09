{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module: Test.Semiring
Description: Some QuickCheck properties for Semirings
License: MIT
Maintainer: mail@doisinkidney.com
Stability: experimental
-}

module Test.Semiring
  ( plusAssoc
  , mulAssoc
  , plusComm
  , mulDistribL
  , mulDistribR
  , plusId
  , mulId
  , annihilate
  , unaryLaws
  , binaryLaws
  , ternaryLaws
  , starLaw
  , plusLaw
  , starLaws
  , nearTernaryLaws
  , ordLaws
  , zeroLaw
  , zeroIsZero
  , zeroLaws
  , nearUnaryLaws
  ) where

import           Data.Semiring   (Semiring (..), StarSemiring (..), DetectableZero(..))

-- | Plus is associative.
plusAssoc :: (Eq a, Semiring a, Show a) => a -> a -> a -> Either String String
plusAssoc x y z = if res then Right s else Left s where
  res = lp == rp
  l = x <+> y
  r = y <+> z
  lp = l <+> z
  rp = x <+> r
  s = unlines
    [ "<+> is " ++ (if res then "" else "not ") ++ "associative."
    , "    Law:"
    , "        (x <+> y) <+> z = x <+> (y <+> z)"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z
    , "    x <+> y = " ++ show l
    , "    y <+> z = " ++ show r
    , "    (x <+> y) <+> z = " ++ show lp
    , "    x <+> (y <+> z) = " ++ show rp ]

-- | Multiplication is associative.
mulAssoc :: (Eq a, Semiring a, Show a) => a -> a -> a -> Either String String
mulAssoc x y z = if res then Right s else Left s  where
  res = lp == rp
  l = x <.> y
  r = y <.> z
  lp = l <.> z
  rp = x <.> r
  s = unlines
    [ "<+> is " ++ (if res then "" else "not ") ++ "associative."
    , "    Law:"
    , "        (x <.> y) <.> z = x <.> (y <.> z)"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z
    , "    x <.> y = " ++ show l
    , "    y <.> z = " ++ show r
    , "    (x <.> y) <.> z = " ++ show lp
    , "    x <.> (y <.> z) = " ++ show rp]

-- | Plus is commutative.
plusComm :: (Eq a, Semiring a, Show a) => a -> a -> Either String String
plusComm x y = if res then Right s else Left s where
  res = l == r
  l = x <+> y
  r = y <+> x
  s = unlines
    [ "<+> is " ++ (if res then "" else "not ") ++ "commutative."
    , "    Law:"
    , "        x <+> y = y <+> x"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    x <+> y = " ++ show l
    , "    y <+> x = " ++ show r ]

-- | Multiplication distributes left.
mulDistribL :: (Eq a, Semiring a, Show a) => a -> a -> a -> Either String String
mulDistribL x y z = if res then Right s else Left s where
  res = l == r
  l = x <.> (y <+> z)
  r = x <.> y <+> x <.> z
  s = unlines
    [ "<.> does " ++ (if res then "" else "not ") ++ "distribute left over <+>."
    , "    Law:"
    , "        x <.> (y <+> z) = x <.> y <+> x <.> z"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z
    , "    x <.> (y <+> z) = " ++ show l
    , "    x <.> y <+> x <.> z  = " ++ show r ]

-- | Multiplication distributes right.
mulDistribR :: (Eq a, Semiring a, Show a) => a -> a -> a -> Either String String
mulDistribR x y z = if res then Right s else Left s where
  res = l == r
  l = (x <+> y) <.> z
  r = x <.> z <+> y <.> z
  s = unlines
    [ "<.> does " ++ (if res then "" else "not ") ++ "distribute right over <+>."
    , "    Law:"
    , "        (x <+> y) <.> z = x <.> z <+> y <.> z"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z
    , "    (x <+> y) <.> z = " ++ show l
    , "    x <.> z <+> y <.> z = " ++ show r ]

-- | Additive identity.
plusId :: (Eq a, Semiring a, Show a) => a -> Either String String
plusId (x :: a) = if res then Right s else Left s where
  res = l == x && r ==x
  l = x <+> zero
  r = zero <+> x
  s = unlines
    [ "zero is" ++ (if res then "" else " not") ++ " the identity of <+>."
    , "    Law:"
    , "        x <+> zero = zero <+> x = x"
    , "    x = " ++ show x
    , "    zero = " ++ show (zero :: a)
    , "    x <+> zero = " ++ show l
    , "    zero <+> x = " ++ show r ]

-- | Multiplicative identity.
mulId :: (Eq a, Semiring a, Show a) => a -> Either String String
mulId (x :: a) = if res then Right s else Left s where
  res = l == x && r == x
  l = x <.> one
  r = one <.> x
  s = unlines
    [ "one is" ++ (if res then "" else " not") ++ " the identity of <+>."
    , "    Law:"
    , "        x <.> one = one <.> x = x"
    , "    x = " ++ show x
    , "    one = " ++ show (one :: a)
    , "    x <.> one = " ++ show l
    , "    one <.> x = " ++ show r ]

-- | Annihilation of '<.>' by 'zero'.
annihilate :: (Eq a, Semiring a, Show a) => a -> Either String String
annihilate (x :: a) = if res then Right s else Left s where
  res = l == zero && r == zero
  l = x <.> zero
  r = zero <.> x
  s = unlines
    [ "zero does " ++ (if res then "" else "not ") ++ "annihilate with <.>."
    , "    Law:"
    , "        x <.> zero = zero <.> x = zero"
    , "    x = " ++ show x
    , "    zero = " ++ show (zero :: a)
    , "    x <.> zero = " ++ show l
    , "    zero <.> x = " ++ show r ]

unaryLaws :: (Eq a, Semiring a, Show a) => a -> Either String String
unaryLaws x = fmap unlines (sequence [plusId x, mulId x, annihilate x])

nearUnaryLaws :: (Eq a, Semiring a, Show a) => a -> Either String String
nearUnaryLaws x = fmap unlines (sequence [plusId x, annihilate x])

nearTernaryLaws :: (Eq a, Semiring a, Show a)
            => a -> a -> a -> Either String String
nearTernaryLaws x y z =
  fmap unlines (sequence [ plusAssoc x y z
                         , mulAssoc x y z
                         , mulDistribR x y z])

binaryLaws :: (Eq a, Semiring a, Show a)
           => a -> a -> Either String String
binaryLaws = plusComm

ternaryLaws :: (Eq a, Semiring a, Show a)
            => a -> a -> a -> Either String String
ternaryLaws x y z =
  fmap unlines (sequence [ plusAssoc x y z
                         , mulAssoc x y z
                         , mulDistribL x y z
                         , mulDistribR x y z])

starLaw :: (Eq a, StarSemiring a, Show a) => a -> Either String String
starLaw (x :: a) = if res then Right s else Left s where
  res = l == st && r == st
  l = one <+> x <.> star x
  r = one <+> star x <.> x
  st = star x
  s = unlines
    [ "star law" ++ (if res then "" else " not") ++ " followed."
    , "    Law:"
    , "        star x = one <+> x <.> star x = one <+> star x <.> x"
    , "    x = " ++ show x
    , "    one = " ++ show (one :: a)
    , "    star x = " ++ show st
    , "    one <+> x <.> star x = " ++ show l
    , "    one <+> star x <.> x = " ++ show r ]

plusLaw :: (Eq a, StarSemiring a, Show a) => a -> Either String String
plusLaw (x :: a) = if res then Right s else Left s where
  res = r == st
  r = x <.> star x
  st = plus x
  s = unlines
    [ "plus law" ++ (if res then "" else " not") ++ " followed."
    , "    Law:"
    , "        plus x = x <.> star x"
    , "    x = " ++ show x
    , "    star x = " ++ show st
    , "    x <.> star x = " ++ show r ]

starLaws :: (Eq a, StarSemiring a, Show a) => a -> Either String String
starLaws x = fmap unlines (sequence [starLaw x, plusLaw x])


ordAddLaw :: (Ord a, Semiring a, Show a) => a -> a -> a -> Either String String
ordAddLaw (x :: a) (y :: a) (z :: a) = if res then Right s else Left s where
  cnd = x <= y
  lhs = x <+> z <= y <+> z
  rhs = z <+> x <= z <+> y
  res = not cnd || lhs && rhs
  s = unlines
    [ "ordering law" ++ (if res then "" else " not") ++ " followed."
    , "    Law:"
    , "        x <= y => x <+> z <= y <+> z && z <+> x <= z <+> y"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z ]

ordMulLaw :: (Ord a, Semiring a, Show a) => a -> a -> a -> Either String String
ordMulLaw (x :: a) (y :: a) (z :: a) = if res then Right s else Left s where
  cnd = x <= y && zero <= z
  lhs = x <.> z <= y <.> z
  rhs = z <.> x <= z <.> y
  res = not cnd || lhs && rhs
  s = unlines
    [ "ordering law" ++ (if res then "" else " not") ++ " followed."
    , "    Law:"
    , "        x <= y => x <.> z <= y <.> z && z <.> x <= z <.> y"
    , "    x = " ++ show x
    , "    y = " ++ show y
    , "    z = " ++ show z ]


ordLaws :: (Ord a, Semiring a, Show a) => a -> a -> a -> Either String String
ordLaws x y z = fmap unlines (sequence [ordAddLaw x y z, ordMulLaw x y z])


zeroLaw :: (Eq a, DetectableZero a, Show a) => a -> Either String String
zeroLaw (x :: a) = if res then Right s else Left s where
  lhs_1 = x == zero
  lhs_2 = zero == x
  rhs = isZero x
  res = lhs_1 == rhs && lhs_2 == rhs
  s = unlines
    [ "zero law" ++ (if res then "" else " not") ++ " followed."
    , "    Law:"
    , "        x == zero = zero == x = isZero x"
    , "    x = " ++ show x
    , "    x == zero = " ++ show lhs_1
    , "    zero == x = " ++ show lhs_2
    , "    isZero x  = " ++ show rhs ]

zeroIsZero :: (DetectableZero a, Show a) => f a -> Either String String
zeroIsZero (_ :: f a) = if res then Right s else Left s where
  z = zero :: a
  res = isZero z
  s = unlines
    [ "zero is zero law" ++ (if res then "" else " not") ++ " followed."
    , "    Law:"
    , "        isZero zero = True"
    , "    zero = " ++ show z
    , "    isZero zero = " ++ show res ]

zeroLaws :: (DetectableZero a, Show a, Eq a) => a -> Either String String
zeroLaws x = zeroLaw x *> zeroIsZero [x]
