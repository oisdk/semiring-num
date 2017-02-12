{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module: Test.Semiring
Description: Some functions for generating tests for 'Semiring's.
License: MIT
Maintainer: mail@doisinkidney.com
Stability: experimental

This module provides functions which can be quickly converted into
<https://hackage.haskell.org/package/smallcheck smallcheck> or
<https://hackage.haskell.org/package/QuickCheck QuickCheck>-like properties.
The functions are of the form:

> a -> Either String String

where the left case is failure of the test, and the right case is success.

For smallcheck, this function can be used directly as a property:

> smallCheck 10 (plusId :: UnaryLaws Integer)

(the typealias is provided as well)

For QuickCheck, you might want to provide an instance like this:

> instance Testable (Either String String) where
>   property = either (`counterexample` False) (const (property True))

And then testing is as simple as:

> quickCheck (plusAssoc :: TernaryLaws Integer)

There are also functions provided to test multiple laws at once. Putting all of
this together, writing a test for all the semiring laws for, say, 'Integer'
looks like this:

> quickCheck (unaryLaws   :: UnaryLaws   Integer)
> quickCheck (binaryLaws  :: BinaryLaws  Integer)
> quickCheck (ternaryLaws :: TernaryLaws Integer)
-}
module Test.Semiring
  (
   -- * Type Aliases
   UnaryLaws
  ,BinaryLaws
  ,TernaryLaws
  ,
   -- * Semiring Laws
   -- ** Unary
   plusId
  ,mulId
  ,annihilateL
  ,annihilateR
  ,unaryLaws
  ,
   -- ** Binary
   plusComm
  ,binaryLaws
  ,
   -- ** Ternary
   plusAssoc
  ,mulAssoc
  ,mulDistribL
  ,mulDistribR
  ,ternaryLaws
  ,
   -- * Near-semiring laws
   -- ** Unary
   nearUnaryLaws
  ,
   -- ** Ternary
   nearTernaryLaws
  ,
   -- * StarSemiring Laws
   -- ** Unary
   starLaw
  ,plusLaw
  ,starLaws
  ,
   -- * DetectableZero Laws
   -- ** Unary
   zeroLaw
  ,zeroIsZero
  ,zeroLaws
  ,
   -- * Ordering Laws
   -- ** Ternary
   ordMulLaw
  ,ordAddLaw
  ,ordLaws)
  where

import Data.Semiring
       (Semiring(..), StarSemiring(..), DetectableZero(..))

-- | Typealias for unary laws. Can be used like so:
--
-- > smallCheck 10 (unaryLaws :: UnaryLaws Int)
type UnaryLaws a = a -> Either String String

-- | Typealias for binary laws. Can be used like so:
--
-- > smallCheck 8 (binaryLaws :: BinaryLaws Int)
type BinaryLaws a = a -> a -> Either String String

-- | Typealias for ternary laws. Can be used like so:
--
-- > smallCheck 6 (ternaryLaws :: TernaryLaws Int)
type TernaryLaws a = a -> a -> a -> Either String String

-- | Plus is associative.
--
-- @(x '<+>' y) '<+>' z = x '<+>' (y '<+>' z)@
plusAssoc
    :: (Eq a, Semiring a, Show a)
    => a -> a -> a -> Either String String
plusAssoc x y z =
    if res
        then Right s
        else Left s
  where
    res = lp == rp
    l = x <+> y
    r = y <+> z
    lp = l <+> z
    rp = x <+> r
    s =
        unlines
            [ "<+> is " ++
              (if res
                   then ""
                   else "not ") ++
              "associative."
            , "    Law:"
            , "        (x <+> y) <+> z = x <+> (y <+> z)"
            , "    x = " ++ show x
            , "    y = " ++ show y
            , "    z = " ++ show z
            , "    x <+> y = " ++ show l
            , "    y <+> z = " ++ show r
            , "    (x <+> y) <+> z = " ++ show lp
            , "    x <+> (y <+> z) = " ++ show rp]

-- | Multiplication is associative.
--
-- @(x '<.>' y) '<.>' z = x '<.>' (y '<.>' z)@
mulAssoc
    :: (Eq a, Semiring a, Show a)
    => a -> a -> a -> Either String String
mulAssoc x y z =
    if res
        then Right s
        else Left s
  where
    res = lp == rp
    l = x <.> y
    r = y <.> z
    lp = l <.> z
    rp = x <.> r
    s =
        unlines
            [ "<+> is " ++
              (if res
                   then ""
                   else "not ") ++
              "associative."
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
--
-- @x '<+>' y = y '<+>' x@
plusComm
    :: (Eq a, Semiring a, Show a)
    => a -> a -> Either String String
plusComm x y =
    if res
        then Right s
        else Left s
  where
    res = l == r
    l = x <+> y
    r = y <+> x
    s =
        unlines
            [ "<+> is " ++
              (if res
                   then ""
                   else "not ") ++
              "commutative."
            , "    Law:"
            , "        x <+> y = y <+> x"
            , "    x = " ++ show x
            , "    y = " ++ show y
            , "    x <+> y = " ++ show l
            , "    y <+> x = " ++ show r]

-- | Multiplication distributes left.
--
-- @x '<.>' (y '<+>' z) = x '<.>' y '<+>' x '<.>' z@
mulDistribL
    :: (Eq a, Semiring a, Show a)
    => a -> a -> a -> Either String String
mulDistribL x y z =
    if res
        then Right s
        else Left s
  where
    res = l == r
    l = x <.> (y <+> z)
    r = x <.> y <+> x <.> z
    s =
        unlines
            [ "<.> does " ++
              (if res
                   then ""
                   else "not ") ++
              "distribute left over <+>."
            , "    Law:"
            , "        x <.> (y <+> z) = x <.> y <+> x <.> z"
            , "    x = " ++ show x
            , "    y = " ++ show y
            , "    z = " ++ show z
            , "    x <.> (y <+> z) = " ++ show l
            , "    x <.> y <+> x <.> z  = " ++ show r]

-- | Multiplication distributes right.
--
-- @(x '<+>' y) '<.>' z = x '<.>' z '<+>' y '<.>' z@
mulDistribR
    :: (Eq a, Semiring a, Show a)
    => a -> a -> a -> Either String String
mulDistribR x y z =
    if res
        then Right s
        else Left s
  where
    res = l == r
    l = (x <+> y) <.> z
    r = x <.> z <+> y <.> z
    s =
        unlines
            [ "<.> does " ++
              (if res
                   then ""
                   else "not ") ++
              "distribute right over <+>."
            , "    Law:"
            , "        (x <+> y) <.> z = x <.> z <+> y <.> z"
            , "    x = " ++ show x
            , "    y = " ++ show y
            , "    z = " ++ show z
            , "    (x <+> y) <.> z = " ++ show l
            , "    x <.> z <+> y <.> z = " ++ show r]

-- | Additive identity.
--
-- @x '<+>' 'zero' = 'zero' '<+>' x = x@
plusId
    :: (Eq a, Semiring a, Show a)
    => a -> Either String String
plusId (x :: a) =
    if res
        then Right s
        else Left s
  where
    res = l == x && r == x
    l = x <+> zero
    r = zero <+> x
    s =
        unlines
            [ "zero is" ++
              (if res
                   then ""
                   else " not") ++
              " the identity of <+>."
            , "    Law:"
            , "        x <+> zero = zero <+> x = x"
            , "    x = " ++ show x
            , "    zero = " ++ show (zero :: a)
            , "    x <+> zero = " ++ show l
            , "    zero <+> x = " ++ show r]

-- | Multiplicative identity.
--
-- @x '<.>' 'one' = 'one' '<.>' x = x@
mulId
    :: (Eq a, Semiring a, Show a)
    => a -> Either String String
mulId (x :: a) =
    if res
        then Right s
        else Left s
  where
    res = l == x && r == x
    l = x <.> one
    r = one <.> x
    s =
        unlines
            [ "one is" ++
              (if res
                   then ""
                   else " not") ++
              " the identity of <+>."
            , "    Law:"
            , "        x <.> one = one <.> x = x"
            , "    x = " ++ show x
            , "    one = " ++ show (one :: a)
            , "    x <.> one = " ++ show l
            , "    one <.> x = " ++ show r]

-- | Right annihilation of '<.>' by 'zero'.
--
-- @'zero' '<.>' x = 'zero'@
annihilateR
    :: (Eq a, Semiring a, Show a)
    => a -> Either String String
annihilateR (x :: a) =
    if res
        then Right s
        else Left s
  where
    res = r == zero
    r = zero <.> x
    s =
        unlines
            [ "zero does " ++
              (if res
                   then ""
                   else "not ") ++
              "annihilate right with <.>."
            , "    Law:"
            , "        zero <.> x = zero"
            , "    x = " ++ show x
            , "    zero = " ++ show (zero :: a)
            , "    zero <.> x = " ++ show r]

-- | Left annihilation of '<.>' by 'zero'.
--
-- @x '<.>' 'zero' = 'zero'@
annihilateL
    :: (Eq a, Semiring a, Show a)
    => a -> Either String String
annihilateL (x :: a) =
    if res
        then Right s
        else Left s
  where
    res = l == zero
    l = x <.> zero
    s =
        unlines
            [ "zero does " ++
              (if res
                   then ""
                   else "not ") ++
              "annihilate left with <.>."
            , "    Law:"
            , "        x <.> zero = zero"
            , "    x = " ++ show x
            , "    zero = " ++ show (zero :: a)
            , "    x <.> zero = " ++ show l]

-- | A test for all three unary laws for 'Semiring's ('plusId', 'mulId',
-- 'annihilateL', and 'annihilateR').
unaryLaws
    :: (Eq a, Semiring a, Show a)
    => a -> Either String String
unaryLaws x =
    fmap unlines (sequence [plusId x, mulId x, annihilateL x, annihilateR x])

-- | A test for the unary laws for near-'Semiring's ('plusId', 'mulId', and
-- 'annihilateR').
nearUnaryLaws
    :: (Eq a, Semiring a, Show a)
    => a -> Either String String
nearUnaryLaws x = fmap unlines (sequence [plusId x, mulId x, annihilateR x])

-- | A test for all of the ternary laws for near-'Semiring's ('plusAssoc',
-- 'mulAssoc', 'mulDistribR').
nearTernaryLaws
    :: (Eq a, Semiring a, Show a)
    => a -> a -> a -> Either String String
nearTernaryLaws x y z =
    fmap
        unlines
        (sequence [plusAssoc x y z, mulAssoc x y z, mulDistribR x y z])

-- | A test for all of the binary laws for 'Semiring's (just 'plusComm').
binaryLaws
    :: (Eq a, Semiring a, Show a)
    => a -> a -> Either String String
binaryLaws = plusComm

-- | A test for all of the ternary laws for 'Semiring's ('plusAssoc', 'mulAssoc',
-- 'mulDistribL', 'mulDistribR').
ternaryLaws
    :: (Eq a, Semiring a, Show a)
    => a -> a -> a -> Either String String
ternaryLaws x y z =
    fmap
        unlines
        (sequence
             [ plusAssoc x y z
             , mulAssoc x y z
             , mulDistribR x y z
             , mulDistribL x y z])

-- | The star law for 'StarSemiring's.
--
-- @'star' x = 'one' '<+>' x '<.>' 'star' x = 'one' '<+>' 'star' x '<.>' x@
starLaw
    :: (Eq a, StarSemiring a, Show a)
    => a -> Either String String
starLaw (x :: a) =
    if res
        then Right s
        else Left s
  where
    res = l == st && r == st
    l = one <+> x <.> star x
    r = one <+> star x <.> x
    st = star x
    s =
        unlines
            [ "star law" ++
              (if res
                   then ""
                   else " not") ++
              " followed."
            , "    Law:"
            , "        star x = one <+> x <.> star x = one <+> star x <.> x"
            , "    x = " ++ show x
            , "    one = " ++ show (one :: a)
            , "    star x = " ++ show st
            , "    one <+> x <.> star x = " ++ show l
            , "    one <+> star x <.> x = " ++ show r]

-- | The plus law for 'StarSemiring's.
--
-- @'plus' x = x '<.>' 'star' x@
plusLaw
    :: (Eq a, StarSemiring a, Show a)
    => a -> Either String String
plusLaw (x :: a) =
    if res
        then Right s
        else Left s
  where
    res = r == st
    r = x <.> star x
    st = plus x
    s =
        unlines
            [ "plus law" ++
              (if res
                   then ""
                   else " not") ++
              " followed."
            , "    Law:"
            , "        plus x = x <.> star x"
            , "    x = " ++ show x
            , "    star x = " ++ show st
            , "    x <.> star x = " ++ show r]

-- | The laws for 'StarSemiring's ('starLaw', 'plusLaw').
starLaws
    :: (Eq a, StarSemiring a, Show a)
    => a -> Either String String
starLaws x = fmap unlines (sequence [starLaw x, plusLaw x])

-- | Addition law for ordered 'Semiring's.
--
-- @x '<=' y => x '<+>' z '<=' y '<+>' z '&&' z '<+>' x '<=' z '<+>' y@
ordAddLaw
    :: (Ord a, Semiring a, Show a)
    => a -> a -> a -> Either String String
ordAddLaw (x :: a) (y :: a) (z :: a) =
    if res
        then Right s
        else Left s
  where
    cnd = x <= y
    lhs = x <+> z <= y <+> z
    rhs = z <+> x <= z <+> y
    res = not cnd || lhs && rhs
    s =
        unlines
            [ "additive ordering law" ++
              (if res
                   then ""
                   else " not") ++
              " followed."
            , "    Law:"
            , "        x <= y => x <+> z <= y <+> z && z <+> x <= z <+> y"
            , "    x = " ++ show x
            , "    y = " ++ show y
            , "    z = " ++ show z]

-- | Multiplication law for ordered 'Semiring's.
--
-- @x '<=' y => x '<.>' z '<=' y '<.>' z '&&' z '<.>' x '<=' z '<.>' y@
ordMulLaw
    :: (Ord a, Semiring a, Show a)
    => a -> a -> a -> Either String String
ordMulLaw (x :: a) (y :: a) (z :: a) =
    if res
        then Right s
        else Left s
  where
    cnd = x <= y && zero <= z
    lhs = x <.> z <= y <.> z
    rhs = z <.> x <= z <.> y
    res = not cnd || lhs && rhs
    s =
        unlines
            [ "ordering law" ++
              (if res
                   then ""
                   else " not") ++
              " followed."
            , "    Law:"
            , "        x <= y => x <.> z <= y <.> z && z <.> x <= z <.> y"
            , "    x = " ++ show x
            , "    y = " ++ show y
            , "    z = " ++ show z]

-- | Laws for ordered 'Semiring's ('ordMulLaw', 'ordAddLaw').
ordLaws
    :: (Ord a, Semiring a, Show a)
    => a -> a -> a -> Either String String
ordLaws x y z = fmap unlines (sequence [ordAddLaw x y z, ordMulLaw x y z])

-- | Law for result of 'isZero' operation.
--
-- @x '==' 'zero' = 'zero' '==' x = 'isZero' x@
zeroLaw
    :: (Eq a, DetectableZero a, Show a)
    => a -> Either String String
zeroLaw (x :: a) =
    if res
        then Right s
        else Left s
  where
    lhs_1 = x == zero
    lhs_2 = zero == x
    rhs = isZero x
    res = lhs_1 == rhs && lhs_2 == rhs
    s =
        unlines
            [ "zero law" ++
              (if res
                   then ""
                   else " not") ++
              " followed."
            , "    Law:"
            , "        x == zero = zero == x = isZero x"
            , "    x = " ++ show x
            , "    x == zero = " ++ show lhs_1
            , "    zero == x = " ++ show lhs_2
            , "    isZero x  = " ++ show rhs]

-- | Zero is zero law.
--
-- @'isZero' 'zero' = 'True'@
zeroIsZero
    :: (DetectableZero a, Show a)
    => f a -> Either String String
zeroIsZero (_ :: f a) =
    if res
        then Right s
        else Left s
  where
    z = zero :: a
    res = isZero z
    s =
        unlines
            [ "zero is zero law" ++
              (if res
                   then ""
                   else " not") ++
              " followed."
            , "    Law:"
            , "        isZero zero = True"
            , "    zero = " ++ show z
            , "    isZero zero = " ++ show res]

-- | The laws for 'DetectableZero' 'Semiring's ('zeroLaw', 'zeroIsZero').
zeroLaws
    :: (DetectableZero a, Show a, Eq a)
    => a -> Either String String
zeroLaws x = zeroLaw x *> zeroIsZero [x]
