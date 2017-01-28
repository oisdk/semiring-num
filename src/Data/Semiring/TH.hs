module Data.Semiring.TH where

import Control.Monad
import Language.Haskell.TH

cmbN :: Int -> Q Exp -> Q Exp
cmbN n f' = do
  f <- f'
  xs <- replicateM n (newName "x")
  ys <- replicateM n (newName "y")
  let args = [TupP (map VarP xs), TupP (map VarP ys)]
      ntup = TupE (zipWith (AppE . AppE f) (map VarE xs) (map VarE ys))
  pure (LamE args ntup)

appN :: Int -> Q Exp -> Q Exp
appN n f' = do
  f <- f'
  xs <- replicateM n (newName "x")
  let args = [TupP (map VarP xs)]
      ntup = TupE (map (AppE f . VarE) xs)
  pure (LamE args ntup)

repN :: Int -> Q Exp -> Q Exp
repN n = fmap TupE . replicateM n
