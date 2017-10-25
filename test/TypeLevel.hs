{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PolyKinds #-}

module TypeLevel where

import           Data.Proxy
import           GHC.Exts

type family AllAre (c :: k -> Constraint) (xs :: [k]) :: Constraint where
        AllAre c '[] = ()
        AllAre c (x ': xs) = (c x, AllAre c xs)

class Reifiable (xs :: [k]) where
    reify
        :: AllAre c xs
        => Proxy c
        -> Proxy xs
        -> (forall a. c a =>
                      Proxy a -> b)
        -> [b]

instance Reifiable '[] where
    reify _ _ _ = []

instance Reifiable xs =>
         Reifiable (x ': xs) where
    reify p _ f = f (Proxy :: Proxy x) : reify p (Proxy :: Proxy xs) f

class (c a, d a) => (c && d) a where
instance (c a, d a) => (c && d) a
