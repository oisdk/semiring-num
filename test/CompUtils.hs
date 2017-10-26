module CompUtils where

import Data.Coerce

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

infixr 9 #.
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> a -> c
(#.) _ = coerce

infixr 9 .#
(.#) :: Coercible a b => (b -> c) -> (a -> b) -> a -> c
(.#) f _ = coerce f

infixl 4 <#$>
(<#$>) :: Coercible (f a) (f b) => (a -> b) -> f a -> f b
(<#$>) _ = coerce

infixr 9 |.|
(|.|) :: (b -> c) -> (a -> b) -> a -> c
(|.|) f g x = f (g x)
{-# NOINLINE (|.|) #-}
