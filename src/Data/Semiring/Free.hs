-- | The Free semiring.
module Data.Semiring.Free
  (Free(..)
  ,liftFree
  ,lowerFree
  ,runFree)
  where

import           Data.Coerce
import           Data.Semiring

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Numeric.Natural

-- | The free semiring
newtype Free a = Free
  { getFree :: Map [a] Natural
  } deriving (Show, Read, Eq, Ord)

instance Ord a =>
         Semiring (Free a) where
    Free xs <+> Free ys = Free (Map.unionWith (+) xs ys)
    {-# INLINE (<+>) #-}
    Free xs <.> Free ys =
        Free
            (Map.foldlWithKey'
                 (\acc k v ->
                       combScale v acc (Map.mapKeysMonotonic (k ++) ys))
                 Map.empty
                 xs)
    {-# INLINE (<.>) #-}
    one = Free (Map.singleton [] 1)
    {-# INLINE one #-}
    zero = Free Map.empty
    {-# INLINE zero #-}

instance Ord a => Num (Free a) where
    fromInteger = Free . Map.singleton [] . fromInteger
    {-# INLINE fromInteger #-}
    (+) = (<+>)
    {-# INLINE (+) #-}
    (*) = (<.>)
    {-# INLINE (*) #-}
    abs = id
    {-# INLINE abs #-}
    signum (Free x) = if Map.null x then zero else one
    {-# INLINE signum #-}
    negate = id
    {-# INLINE negate #-}

-- | Run a 'Free'.
runFree :: Semiring s => (a -> s) -> Free a -> s
runFree f = getAdd .# Map.foldMapWithKey ((rep #. Add) . mul . map f) . getFree
{-# INLINE runFree #-}

-- | Run a 'Free', interpreting it in the underlying semiring.
lowerFree :: Semiring s => Free s -> s
lowerFree = runFree id
{-# INLINE lowerFree #-}

liftFree :: a -> Free a
liftFree = Free . flip Map.singleton one . pure

infixr 9 #.
(#.) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(#.) f _ = coerce f
{-# INLINE (#.) #-}

infixr 9 .#
(.#) :: Coercible b c => (b -> c) -> (a  -> b) -> a -> c
(.#) _ = coerce
{-# INLINE (.#) #-}

combScale
    :: (Ord a, Semiring b)
    => b -> Map a b -> Map a b -> Map a b
combScale p =
    Map.mergeWithKey
         (\_ x y ->
               Just $ x <+> (p <.> y))
         id
         (Map.map (p <.>))
{-# INLINE combScale #-}

instance Foldable Free where
    foldMap f (Free xs) = Map.foldMapWithKey (rep . foldMap f) xs
    length (Free xs) = Map.foldl' (\a e -> a + fromIntegral e) 0 xs

rep :: Monoid m => m -> Natural -> m
rep x = go
  where
    go 0 = mempty
    go 1 = x
    go n
      | even n = r `mappend` r
      | otherwise = x `mappend` r `mappend` r
      where
        r = go (n `div` 2)
{-# INLINE rep #-}
