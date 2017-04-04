module Data.Semiring.TH where

import Control.Monad
import Language.Haskell.TH

typeNames :: Int -> Q [Name]
typeNames = traverse newName . map pure . flip take ['a'..]

varNames :: Int -> Q [Name]
varNames n =
    (traverse newName . map pure . reverse . take n . reverse . take 26)
        ['a' ..]

repN :: Int -> String -> Q Dec
repN n nm = do
    let v = VarP (mkName nm)
    rhs <- TupE <$> replicateM n (pure (VarE (mkName nm)))
    return $ ValD v (NormalB rhs) []

appN :: Int -> String -> Q Dec
appN n nm = do
    let f = VarE (mkName nm)
    xs <- varNames n
    let args = [TupP (map VarP xs)]
        ntup = TupE (map (AppE f . VarE) xs)
    return $ FunD (mkName nm) [Clause args (NormalB ntup) []]

cmbN :: Int -> String -> Q Dec
cmbN n nm = do
    let f = VarE (mkName nm)
    xs <- varNames n
    ys <- varNames n
    let args = [TupP (map VarP xs), TupP (map VarP ys)]
        ntup = TupE (zipWith (AppE . AppE f) (map VarE xs) (map VarE ys))
    return $ FunD (mkName nm) [Clause args (NormalB ntup) []]

starIns :: Int -> Q Dec
starIns n = do
    names <- typeNames n
    let c = ConT (mkName "StarSemiring")
        ct = map (AppT c . VarT) names
    InstanceD Nothing ct (AppT c $ foldl AppT (TupleT n) (map VarT names)) <$>
        sequence [appN n "star", pure (inline "star"), appN n "plus", pure (inline "plus")]

inline :: String -> Dec
inline n = PragmaD (InlineP (mkName n) Inline FunLike AllPhases)

semiringIns :: Int -> Q Dec
semiringIns n = do
    names <- typeNames n
    let c = ConT (mkName "Semiring")
        ct = map (AppT c . VarT) names
    InstanceD Nothing ct (AppT c $ foldl AppT (TupleT n) (map VarT names)) <$>
        sequence
            [ cmbN n "<+>"
            , pure (inline "<+>")
            , cmbN n "<.>"
            , pure (inline "<.>")
            , repN n "zero"
            , pure (inline "zero")
            , repN n "one"
            , pure (inline "one")]

zeroIns :: Int -> Q Dec
zeroIns n = do
    names <- typeNames n
    let c = ConT (mkName "DetectableZero")
        ct = map (AppT c . VarT) names
    InstanceD Nothing ct (AppT c $ foldl AppT (TupleT n) (map VarT names)) <$>
      sequence [andAll n, pure (inline "isZero")]

andAll :: Int -> Q Dec
andAll n = do
  let f = VarE (mkName "&&")
  let isZ = VarE (mkName "isZero")
  xs <- replicateM n (newName "x")
  let args = [TupP (map VarP xs)]
      res = foldl1 (\a e -> AppE (AppE f a) e ) (map (AppE isZ . VarE) xs)
  return $ FunD (mkName "isZero") [Clause args (NormalB res) []]
