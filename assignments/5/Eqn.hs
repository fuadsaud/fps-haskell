module Eqn where

import Data.List (sortBy,nub)
import Data.Ratio

import Exp
import MPoly

data Eqn = Eqn Exp Exp deriving Show

system :: (Eq a, Fractional a) => [Eqn] -> [[Rational]]
system eqns = map tuple2list tuples
    where
        tuples = sortBy firstKVar . map eqn2mpoly $ eqns
        allKVars =  nub . concat . map (kvars . fst) $ tuples
        tuple2list (p, (Const c)) = (mpoly2list p allKVars) ++ [c - sumConst p]
        firstKVar ((ProdPlus (Const _) (KVar k1) _), _)
                  ((ProdPlus (Const _) (KVar k2) _), _) = compare k1 k2
        sumConst (ProdPlus _ _ (Const c)) = c
        sumConst (ProdPlus _ _ p)         = sumConst p


mpoly2list :: MPoly -> [String] -> [Rational]
mpoly2list p = reverse . snd . foldl collectBindings (p, [])
    where
        collectBindings (p1@(ProdPlus (Const c) (KVar k) p2), bindings) kvar
            | kvar == k = (p2, c:bindings)
            | otherwise = (p1, 0:bindings)
        collectBindings (p, bindings) _ = (p, 0:bindings)

kvars :: MPoly -> [String]
kvars (Const _)                       = []
kvars (ProdPlus (Const c) (KVar k) p) = k:kvars p

eqn2mpoly (Eqn lhs rhs) = (fromExp lhs, fromExp rhs)
