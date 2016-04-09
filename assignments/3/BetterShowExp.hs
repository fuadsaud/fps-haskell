module BetterShowExp where

import Data.Ratio
import Data.List (intercalate)

data Exp = RExp Rational   |
           Var  String     |
           Sum  Exp Exp    |
           Prod Exp Exp    |
           Pow  Exp Exp deriving Eq

instance Show Exp where
        show = showSumContext

intEval :: Exp -> Integer
intEval exp = numerator $ ratioEval exp

ratioEval :: Exp -> Rational
ratioEval (RExp n)   = n
ratioEval (Var x)    = error "Variable encountered in rational expression"
ratioEval (Sum u v)  = (ratioEval u) + (ratioEval v)
ratioEval (Prod u v) = (ratioEval u) * (ratioEval v)
ratioEval (Pow u v)  =
        let nv = intEval v
        in if nv >= 0
                then (ratioEval u) ^ nv
                else error "Fraction encountered in rational expression"

addParens :: String -> String
addParens s = "(" ++ s ++ ")"

showNum :: Rational -> String
showNum a
    | denominator a == 1     = show (numerator a)
    | otherwise              =
        "(" ++ (show (numerator a)) ++ "/" ++ (show (denominator a)) ++ ")"

showOperation :: String -> (Exp -> String) -> ([Exp] -> String)
showOperation operator contextF = intercalate operator . map contextF

showSum :: [Exp] -> String
showSum = showOperation "+" showSumContext

showProd :: [Exp] -> String
showProd = showOperation "*" showProdContext

showPow :: (Exp, Exp) -> String
showPow (base, expo) = showPowContextLeft base ++ "^" ++ showPowContextRight expo

showSumContext :: Exp -> String
showSumContext (RExp const)     = showNum const
showSumContext (Sum lexp rexp)  = showSumContext lexp ++ "+" ++ showSumContext rexp
showSumContext (Prod lexp rexp) = showProdContext lexp ++ "*" ++ showProdContext rexp
showSumContext (Pow base expo)  = showPow (base, expo)
showSumContext (Var v)          = v

showProdContext :: Exp -> String
showProdContext (RExp const)     = showNum const
showProdContext (Sum lexp rexp)  = addParens . showSum $ [lexp, rexp]
showProdContext (Prod lexp rexp) = showProd [lexp, rexp]
showProdContext (Pow base expo)  = showPow (base, expo)
showProdContext (Var v)          = v

showPowContextLeft :: Exp -> String
showPowContextLeft (RExp const)
        | const < 0 = addParens . showNum $ const
        | otherwise = showNum const
showPowContextLeft (Sum lexp rexp)  = addParens . showSum $ [lexp, rexp]
showPowContextLeft (Prod lexp rexp) = addParens . showProd $ [lexp, rexp]
showPowContextLeft (Pow base expo)  = addParens . showPow $ (base, expo)
showPowContextLeft (Var v)          = v

showPowContextRight :: Exp -> String
showPowContextRight (RExp const)     = showNum const
showPowContextRight (Sum lexp rexp)  = addParens . showSum $ [lexp, rexp]
showPowContextRight (Prod lexp rexp) = addParens . showProd $ [lexp, rexp]
showPowContextRight (Pow base expo)  = showPow (base, expo)
showPowContextRight (Var v)          = v

deriv :: Exp -> String -> Exp
deriv (RExp n) x         = RExp 0
deriv (Var x) y
    | x == y             = RExp 1
    | otherwise          = RExp 0
deriv (Sum u v) x        = Sum (deriv u x) (deriv v x)
deriv (Prod u v) x       = Sum (Prod u (deriv v x)) (Prod v (deriv u x))
deriv (Pow u (RExp n)) x = (Prod (Prod (RExp n) (Pow u (RExp (n - 1)))) (deriv u x))

visitOnce :: (Exp -> Exp) -> Exp -> Exp
visitOnce f e@(RExp n) = e
visitOnce f e@(Var x)  = e
visitOnce f (Sum u v)  = f (Sum (visitOnce f u) (visitOnce  f v))
visitOnce f (Prod u v) = f (Prod (visitOnce f u) (visitOnce f v))
visitOnce f (Pow u v)  = f (Pow (visitOnce f u) (visitOnce f v))

simp :: Exp -> Exp
simp (Sum (RExp n) (RExp m))  = RExp (n + m)
simp (Sum (RExp 0) v)         = v
simp (Sum u (RExp 0))         = u
simp (Prod (RExp n) (RExp m)) = RExp (n * m)
simp (Prod (RExp 0) v)        = RExp 0
simp (Prod u (RExp 0))        = RExp 0
simp (Prod (RExp 1) v)        = v
simp (Prod u (RExp 1))        = u
simp (Pow u (RExp 0))         = RExp 1
simp (Pow u (RExp 1))         = u
simp u                        = u


simplify1 :: Exp -> Exp
simplify1  = (visitOnce simp)

visitUntilUnchanged :: (Exp -> Exp) -> Exp -> Exp
visitUntilUnchanged f tr = let new = visitOnce f tr
                           in if new == tr
                              then tr
                              else visitUntilUnchanged f new

simp2 :: Exp -> Exp
simp2 (Sum (RExp n) (RExp m))                   = RExp (n+m)
simp2 (Sum (RExp n) v)                          = (Sum v (RExp n))
simp2 (Sum u (RExp 0))                          = u
simp2 (Sum (Sum u (RExp n)) (RExp m))           = Sum u (RExp (n+m))
simp2 (Sum (Sum u (RExp n)) v)                  = Sum (Sum u v) (RExp n)
simp2 (Sum u (Sum v w))                         = Sum (Sum u v) w
simp2 (Sum u v)
    | u == v                                    = Prod (RExp 2) u
simp2 (Sum (Prod (RExp n) u) v)
    | u == v                                    = Prod (RExp (n+1)) u
simp2 (Sum u (Prod (RExp n) v))
    | u == v                                    = Prod (RExp (n+1)) u
simp2 (Sum (Prod (RExp n) u) (Prod (RExp m) v))
    | u == v                                    = Prod (RExp (n+m)) u
simp2 (Prod (RExp n) (RExp m))                  = RExp (n*m)
simp2 (Prod u (RExp n))                         = Prod (RExp n) u
simp2 (Prod (RExp 0) v)                         = RExp 0
simp2 (Prod (RExp 1) v)                         = v
simp2 (Prod (RExp n) (Prod (RExp m) v))         = Prod (RExp (n*m)) v
simp2 (Prod u (Prod (RExp n) v))                = Prod (RExp n) (Prod u v)
simp2 (Prod (Prod u v) w)                       = Prod u (Prod v w)
simp2 (Prod (RExp n) (Sum u v))                 = Sum (Prod (RExp n) u) (Prod (RExp n) v)
simp2 (Pow u (RExp 0))                          = RExp 1
simp2 (Pow u (RExp 1))                          = u
simp2 u                                         = u

simplify2 :: Exp -> Exp
simplify2 = (visitUntilUnchanged simp2)
