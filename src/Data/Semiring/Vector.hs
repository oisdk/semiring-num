{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples       #-}

module Data.Semiring.Vector where

import           Data.Foldable            (foldl', for_)

import           Data.Vector.Primitive    (Vector (..))
import qualified Data.Vector.Primitive    as Vector
import qualified Data.Vector.Unboxed      as Unboxed
import           Data.Vector.Unboxed.Base (Vector (V_Double, V_Int32, V_Int64, V_Int8, V_Float))

import           Data.Primitive

import           Data.Bits

import           GHC.Base
import           GHC.Int
import           GHC.ST

import           Data.Semiring.Newtype

addsimd
    :: (Prim a, Num a)
    => Int
    -> Int
    -> (forall s. MutableByteArray# s -> Int# -> ByteArray# -> Int# -> ByteArray# -> Int# -> ST s ())
    -> Vector.Vector a
    -> Vector.Vector a
    -> Vector.Vector a
addsimd vecp@(I# vecp') mask act = go where
  go (Vector lo@(I# lo') ls lb@(ByteArray lb') :: Vector.Vector a) (Vector ro@(I# ro') rs rb@(ByteArray rb')) =
      Vector
          0
          ns
          (runST
              (do ba <- newByteArray (is * ns)
                  paradd ba d
                  seqadd ba r ms
                  endcpy ba
                  unsafeFreezeByteArray ba))
    where
      cs = compare ls rs
      (ms,ns) =
          case cs of
              LT -> (ls, rs)
              _  -> (rs, ls)
      is = sizeOf (undefined :: a)
      d = unsafeShiftR ms vecp
      r = ms .&. complement mask
      paradd (MutableByteArray bb) j =
          for_ [0 .. j - 1] $
          \(I# j') -> let i' = uncheckedIShiftL# j' vecp' in inline act bb i' lb' lo' rb' ro'
      seqadd ba f t =
          for_ [f .. t - 1] $
          \i ->
              writeByteArray
                  ba
                  i
                  (indexByteArray lb (i + lo) + indexByteArray rb (i + ro) :: a)
      endcpy ba =
          case cs of
              EQ -> pure ()
              LT -> copyByteArray ba (ls * is) rb ((ro + ls) * is) ((rs - ls) * is)
              GT -> copyByteArray ba (rs * is) lb ((lo + rs) * is) ((ls - rs) * is)
{-# INLINE addsimd #-}

addInt32s :: Unboxed.Vector Int32 -> Unboxed.Vector Int32 -> Unboxed.Vector Int32
addInt32s =
    (coerce :: Binary (Vector.Vector Int32) -> Binary (Unboxed.Vector Int32)) $
    addsimd
        3
        7
        (\bb i' lb' lo' rb' ro' ->
              ST
                  (\st ->
                        (# writeInt32ArrayAsInt32X8#
                              bb
                              i'
                              (plusInt32X8#
                                   (indexInt32ArrayAsInt32X8# lb' (i' +# lo'))
                                   (indexInt32ArrayAsInt32X8# rb' (i' +# ro')))
                              st
                        , () #)))

convInt32s :: Unboxed.Vector Int32 -> Unboxed.Vector Int32 -> Unboxed.Vector Int32
convInt32s (V_Int32 lv@(Vector (I# lo') ls (ByteArray lb'))) (V_Int32 rv@(Vector (I# ro') rs (ByteArray rb')))
  | ls == 0 = Unboxed.empty
  | rs == 0 = Unboxed.empty
  | otherwise = Unboxed.generate (ls + rs - 1) f
  where
    f n@(I# n') =
        foldl'
            (\a k -> a + Vector.unsafeIndex lv k * Vector.unsafeIndex rv (n - k))
            (foldl' h 0 [0 .. sz - 1])
            [unsafeShiftL sz 3 + kmin .. kmax]
      where
        !kmin@(I# kmin') = max 0 (n - (rs - 1))
        !kmax = min n (ls - 1)
        !sz = unsafeShiftR ((kmax + 1) - kmin) 3
        h (I32# a') (I# j') =
            let i' = (uncheckedIShiftL# j' 3# +# kmin')
            in case unpackInt32X8#
                        (indexInt32ArrayAsInt32X8#
                             rb'
                             (((n' -# i') -# 7#) +# ro')) of
                   (# x1,x2,x3,x4,x5,x6,x7,x8 #) ->
                       case unpackInt32X8#
                                (timesInt32X8#
                                     (indexInt32ArrayAsInt32X8# lb' (i' +# lo'))
                                     (packInt32X8# (# x8,x7,x6,x5,x4,x3,x2,x1 #))) of
                           (# y1,y2,y3,y4,y5,y6,y7,y8 #) ->
                               I32# (a' +# y1 +# y2 +# y3 +# y4 +# y5 +# y6 +# y7 +# y8)

addInt8s :: Unboxed.Vector Int8 -> Unboxed.Vector Int8 -> Unboxed.Vector Int8
addInt8s =
        (coerce :: Binary (Vector.Vector Int8) -> Binary (Unboxed.Vector Int8)) $
    addsimd
        5
        31
        (\bb i' lb' lo' rb' ro' ->
             ST
                 (\st ->
                       (# writeInt8ArrayAsInt8X32#
                             bb
                             i'
                             (plusInt8X32#
                                  (indexInt8ArrayAsInt8X32# lb' (i' +# lo'))
                                  (indexInt8ArrayAsInt8X32# rb' (i' +# ro')))
                             st
                       , () #)))

convInt8s :: Unboxed.Vector Int8 -> Unboxed.Vector Int8 -> Unboxed.Vector Int8
convInt8s (V_Int8 lv@(Vector (I# lo') ls (ByteArray lb'))) (V_Int8 rv@(Vector (I# ro') rs (ByteArray rb')))
  | ls == 0 = Unboxed.empty
  | rs == 0 = Unboxed.empty
  | otherwise = Unboxed.generate (ls + rs - 1) f
  where
    f n@(I# n') =
        foldl'
            (\a k -> a + Vector.unsafeIndex lv k * Vector.unsafeIndex rv (n - k))
            (foldl' h 0 [0 .. sz - 1])
            [unsafeShiftL sz 5 + kmin .. kmax]
      where
        !kmin@(I# kmin') = max 0 (n - (rs - 1))
        !kmax = min n (ls - 1)
        !sz = unsafeShiftR ((kmax + 1) - kmin) 5
        h (I8# a') (I# j') =
            let i' = (uncheckedIShiftL# j' 5# +# kmin')
            in case unpackInt8X32#
                        (indexInt8ArrayAsInt8X32#
                             rb'
                             (((n' -# i') -# 31#) +# ro')) of
                   (# x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32 #) ->
                       case unpackInt8X32#
                                (timesInt8X32#
                                     (indexInt8ArrayAsInt8X32# lb' (i' +# lo'))
                                     (packInt8X32# (# x32,x31,x30,x29,x28,x27,x26,x25,x24,x23,x22,x21,x20,x19,x18,x17,x16,x15,x14,x13,x12,x11,x10,x9,x8,x7,x6,x5,x4,x3,x2,x1 #))) of
                         (# y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11,y12,y13,y14,y15,y16,y17,y18,y19,y20,y21,y22,y23,y24,y25,y26,y27,y28,y29,y30,y31,y32 #) ->
                             I8# (a' +# y1 +# y2 +# y3 +# y4 +# y5 +# y6 +# y7 +# y8 +# y9 +# y10 +# y11 +# y12 +# y13 +# y14 +# y15 +# y16 +# y17 +# y18 +# y19 +# y20 +# y21 +# y22 +# y23 +# y24 +# y25 +# y26 +# y27 +# y28 +# y29 +# y30 +# y31 +# y32)



addInt64s :: Unboxed.Vector Int64 -> Unboxed.Vector Int64 -> Unboxed.Vector Int64
addInt64s =
  (coerce :: Binary (Vector.Vector Int64) -> Binary (Unboxed.Vector Int64)) $
    addsimd
        2
        3
        (\bb i' lb' lo' rb' ro' ->
             ST
                 (\st ->
                       (# writeInt64ArrayAsInt64X4#
                             bb
                             i'
                             (plusInt64X4#
                                  (indexInt64ArrayAsInt64X4# lb' (i' +# lo'))
                                  (indexInt64ArrayAsInt64X4# rb' (i' +# ro')))
                             st
                       , () #)))

convInt64s :: Unboxed.Vector Int64 -> Unboxed.Vector Int64 -> Unboxed.Vector Int64
convInt64s (V_Int64 lv@(Vector (I# lo') ls (ByteArray lb'))) (V_Int64 rv@(Vector (I# ro') rs (ByteArray rb')))
  | ls == 0 = Unboxed.empty
  | rs == 0 = Unboxed.empty
  | otherwise = Unboxed.generate (ls + rs - 1) f
  where
    f n@(I# n') =
        foldl'
            (\a k -> a + Vector.unsafeIndex lv k * Vector.unsafeIndex rv (n - k))
            (foldl' h 0 [0 .. sz - 1])
            [unsafeShiftL sz 2 + kmin .. kmax]
      where
        !kmin@(I# kmin') = max 0 (n - (rs - 1))
        !kmax = min n (ls - 1)
        !sz = unsafeShiftR ((kmax + 1) - kmin) 2
        h (I64# a') (I# j') =
            let i' = (uncheckedIShiftL# j' 2# +# kmin')
            in case unpackInt64X4#
                        (indexInt64ArrayAsInt64X4#
                             rb'
                            (((n' -# i') -# 3#) +# ro')) of
                   (# x1,x2,x3,x4 #) ->
                       case unpackInt64X4#
                                (timesInt64X4#
                                     (indexInt64ArrayAsInt64X4# lb' (i' +# lo'))
                                     (packInt64X4# (# x4,x3,x2,x1 #))) of
                           (# y1,y2,y3,y4 #) ->
                               I64# (a' +# y1 +# y2 +# y3 +# y4)

addDoubles :: Unboxed.Vector Double -> Unboxed.Vector Double -> Unboxed.Vector Double
addDoubles
  = (coerce :: Binary (Vector.Vector Double) -> Binary (Unboxed.Vector Double)) $
      addsimd
          2
          3
          (\bb i' lb' lo' rb' ro' ->
             ST
                 (\st ->
                       (# writeDoubleArrayAsDoubleX4#
                             bb
                             i'
                             (plusDoubleX4#
                                  (indexDoubleArrayAsDoubleX4# lb' (i' +# lo'))
                                  (indexDoubleArrayAsDoubleX4# rb' (i' +# ro')))
                             st
                       , () #)))

convDoubles :: Unboxed.Vector Double -> Unboxed.Vector Double -> Unboxed.Vector Double
convDoubles (V_Double lv@(Vector (I# lo') ls (ByteArray lb'))) (V_Double rv@(Vector (I# ro') rs (ByteArray rb')))
  | ls == 0 = Unboxed.empty
  | rs == 0 = Unboxed.empty
  | otherwise = Unboxed.generate (ls + rs - 1) f
  where
    f n@(I# n') =
        foldl'
            (\a k -> a + Vector.unsafeIndex lv k * Vector.unsafeIndex rv (n - k))
            (foldl' h 0 [0 .. sz - 1])
            [unsafeShiftL sz 2 + kmin .. kmax]
      where
        !kmin@(I# kmin') = max 0 (n - (rs - 1))
        !kmax = min n (ls - 1)
        !sz = unsafeShiftR ((kmax + 1) - kmin) 2
        h (D# a') (I# j') =
            let i' = (uncheckedIShiftL# j' 2# +# kmin')
            in case unpackDoubleX4#
                        (indexDoubleArrayAsDoubleX4#
                             rb'
                            (((n' -# i') -# 3#) +# ro')) of
                   (# x1,x2,x3,x4 #) ->
                       case unpackDoubleX4#
                                (timesDoubleX4#
                                     (indexDoubleArrayAsDoubleX4# lb' (i' +# lo'))
                                     (packDoubleX4# (# x4,x3,x2,x1 #))) of
                           (# y1,y2,y3,y4 #) ->
                               D# (a' +## y1 +## y2 +## y3 +## y4)

addFloats :: Unboxed.Vector Float -> Unboxed.Vector Float -> Unboxed.Vector Float
addFloats
  = (coerce :: Binary (Vector.Vector Float) -> Binary (Unboxed.Vector Float)) $
      addsimd
          3
          7
          (\bb i' lb' lo' rb' ro' ->
             ST
                 (\st ->
                       (# writeFloatArrayAsFloatX8#
                             bb
                             i'
                             (plusFloatX8#
                                  (indexFloatArrayAsFloatX8# lb' (i' +# lo'))
                                  (indexFloatArrayAsFloatX8# rb' (i' +# ro')))
                             st
                       , () #)))

convFloats :: Unboxed.Vector Float -> Unboxed.Vector Float -> Unboxed.Vector Float
convFloats (V_Float lv@(Vector (I# lo') ls (ByteArray lb'))) (V_Float rv@(Vector (I# ro') rs (ByteArray rb')))
  | ls == 0 = Unboxed.empty
  | rs == 0 = Unboxed.empty
  | otherwise = Unboxed.generate (ls + rs - 1) f
  where
    f n@(I# n') =
        foldl'
            (\a k -> a + Vector.unsafeIndex lv k * Vector.unsafeIndex rv (n - k))
            (foldl' h 0 [0 .. sz - 1])
            [unsafeShiftL sz 3 + kmin .. kmax]
      where
        !kmin@(I# kmin') = max 0 (n - (rs - 1))
        !kmax = min n (ls - 1)
        !sz = unsafeShiftR ((kmax + 1) - kmin) 3
        h (F# a') (I# j') =
            let i' = (uncheckedIShiftL# j' 3# +# kmin')
            in case unpackFloatX8#
                        (indexFloatArrayAsFloatX8#
                             rb'
                            (((n' -# i') -# 7#) +# ro')) of
                   (# x1,x2,x3,x4,x5,x6,x7,x8 #) ->
                       case unpackFloatX8#
                                (timesFloatX8#
                                     (indexFloatArrayAsFloatX8# lb' (i' +# lo'))
                                     (packFloatX8# (# x8,x7,x6,x5,x4,x3,x2,x1 #))) of
                           (# y1,y2,y3,y4,y5,y6,y7,y8 #) ->
                               F# (a' `plusFloat#` y1 `plusFloat#` y2 `plusFloat#` y3 `plusFloat#` y4 `plusFloat#` y5 `plusFloat#` y6 `plusFloat#` y7 `plusFloat#` y8)
