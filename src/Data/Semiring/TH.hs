module Data.Semiring.TH where

import Control.Monad
import Language.Haskell.TH

repN :: Int -> String -> Q Dec
repN n nm = do
    let v = VarP (mkName nm)
    rhs <- TupE <$> replicateM n (pure (VarE (mkName nm)))
    return $ ValD v (NormalB rhs) []

appN :: Int -> String -> Q Dec
appN n nm = do
    let f = VarE (mkName nm)
    xs <- replicateM n (newName "x")
    let args = [TupP (map VarP xs)]
        ntup = TupE (map (AppE f . VarE) xs)
    return $ FunD (mkName nm) [Clause args (NormalB ntup) []]

cmbN :: Int -> String -> Q Dec
cmbN n nm = do
    let f = VarE (mkName nm)
    xs <- replicateM n (newName "x")
    ys <- replicateM n (newName "y")
    let args = [TupP (map VarP xs), TupP (map VarP ys)]
        ntup = TupE (zipWith (AppE . AppE f) (map VarE xs) (map VarE ys))
    return $ FunD (mkName nm) [Clause args (NormalB ntup) []]

starIns :: Int -> Q Dec
starIns n = do
    names <- replicateM n (newName "a")
    let c = ConT (mkName "StarSemiring")
        ct = map (AppT c . VarT) names
    InstanceD Nothing ct (AppT c $ foldl AppT (TupleT n) (map VarT names)) <$>
        sequence [appN n "star", appN n "plus"]

semiringIns :: Int -> Q Dec
semiringIns n = do
    names <- replicateM n (newName "a")
    let c = ConT (mkName "Semiring")
        ct = map (AppT c . VarT) names
    InstanceD Nothing ct (AppT c $ foldl AppT (TupleT n) (map VarT names)) <$>
        sequence [cmbN n "<+>", cmbN n "<.>", repN n "zero", repN n "one"]
