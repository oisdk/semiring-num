{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Semiring.Vector where

import           Data.Foldable            (foldl', for_)

import           Data.Vector.Primitive    (Vector (..))
import qualified Data.Vector.Primitive    as Vector
import qualified Data.Vector.Unboxed      as Unboxed
import           Data.Vector.Unboxed.Base (Vector (V_Int32))

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
    r8 = xor d8 7
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
