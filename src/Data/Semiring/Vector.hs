{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Semiring.Vector where

import           Data.Foldable            (foldl', for_)

import           Data.Vector.Primitive    (Vector (..))
import qualified Data.Vector.Primitive    as Vector
import qualified Data.Vector.Unboxed      as Unboxed
import           Data.Vector.Unboxed.Base (Vector (V_Int32, V_Int8))

import           Data.Primitive

import           Data.Bits

import           GHC.Int
import           GHC.Prim
import           GHC.ST


addInt32s :: Unboxed.Vector Int32 -> Unboxed.Vector Int32 -> Unboxed.Vector Int32
addInt32s (V_Int32 (Vector lo@(I# lo') ls lb@(ByteArray lb'))) (V_Int32 (Vector ro@(I# ro') rs rb@(ByteArray rb'))) =
    V_Int32 (Vector
        0
        ns
        (runST
             (do ba <- newByteArray (is * ns)
                 paradd ba d8
                 seqadd ba r8 ms
                 endcpy ba
                 unsafeFreezeByteArray ba)))
  where
    cs = compare ls rs
    (ms,ns) =
        case cs of
            LT -> (ls, rs)
            _  -> (rs, ls)
    is = sizeOf (undefined :: Int32)
    d8 = unsafeShiftR ms 3
    r8 = ms .&. complement 7
    paradd (MutableByteArray bb) j =
        for_ [0 .. j - 1] $
        \(I# j') -> let i' = uncheckedIShiftL# j' 3# in
             ST
                 (\st ->
                       (# writeInt32ArrayAsInt32X8#
                             bb
                             i'
                             (plusInt32X8#
                                  (indexInt32ArrayAsInt32X8# lb' (i' +# lo'))
                                  (indexInt32ArrayAsInt32X8# rb' (i' +# ro')))
                             st
                       , () #))
    seqadd ba f t =
        for_ [f .. t - 1] $
        \i ->
             writeByteArray
                 ba
                 i
                 (indexByteArray lb (i + lo) + indexByteArray rb (i + ro) :: Int32)
    endcpy ba =
        case cs of
            EQ -> pure ()
            LT -> copyByteArray ba (ls * is) rb ((ro + ls) * is) ((rs - ls) * is)
            GT -> copyByteArray ba (rs * is) lb ((lo + rs) * is) ((ls - rs) * is)

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
addInt8s (V_Int8 (Vector lo@(I# lo') ls lb@(ByteArray lb'))) (V_Int8 (Vector ro@(I# ro') rs rb@(ByteArray rb'))) =
    V_Int8 (Vector
        0
        ns
        (runST
             (do ba <- newByteArray (is * ns)
                 paradd ba d32
                 seqadd ba r32 ms
                 endcpy ba
                 unsafeFreezeByteArray ba)))
  where
    cs = compare ls rs
    (ms,ns) =
        case cs of
            LT -> (ls, rs)
            _  -> (rs, ls)
    is = sizeOf (undefined :: Int8)
    d32 = unsafeShiftR ms 5
    r32 = ms .&. complement 31
    paradd (MutableByteArray bb) j =
        for_ [0 .. j - 1] $
        \(I# j') -> let i' = uncheckedIShiftL# j' 5# in
             ST
                 (\st ->
                       (# writeInt8ArrayAsInt8X32#
                             bb
                             i'
                             (plusInt8X32#
                                  (indexInt8ArrayAsInt8X32# lb' (i' +# lo'))
                                  (indexInt8ArrayAsInt8X32# rb' (i' +# ro')))
                             st
                       , () #))
    seqadd ba f t =
        for_ [f .. t - 1] $
        \i ->
             writeByteArray
                 ba
                 i
                 (indexByteArray lb (i + lo) + indexByteArray rb (i + ro) :: Int8)
    endcpy ba =
        case cs of
            EQ -> pure ()
            LT -> copyByteArray ba (ls * is) rb ((ro + ls) * is) ((rs - ls) * is)
            GT -> copyByteArray ba (rs * is) lb ((lo + rs) * is) ((ls - rs) * is)

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
