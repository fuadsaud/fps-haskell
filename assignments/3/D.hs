module D where

import Data.Ratio
import Data.List (intercalate)

data Exp = RExp Rational |
           Var  String   |
           Sum  Exp Exp  |
           Prod Exp Exp  |
           Pow  Exp Exp  |
           D    Exp String deriving Eq

instance Show Exp where
        show = showSumContext

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

showD :: (Exp, String) -> String
showD (exp, var) = "D(" ++ show exp ++ ", " ++ var ++ ")"

showSumContext :: Exp -> String
showSumContext (RExp const)     = showNum const
showSumContext (Sum lexp rexp)  = showSumContext lexp ++ "+" ++ showSumContext rexp
showSumContext (Prod lexp rexp) = showProdContext lexp ++ "*" ++ showProdContext rexp
showSumContext (Pow base expo)  = showPow (base, expo)
showSumContext (Var v)          = v
showSumContext (D exp var)      = showD (exp, var)

showProdContext :: Exp -> String
showProdContext (RExp const)     = showNum const
showProdContext (Sum lexp rexp)  = addParens . showSum $ [lexp, rexp]
showProdContext (Prod lexp rexp) = showProd [lexp, rexp]
showProdContext (Pow base expo)  = showPow (base, expo)
showProdContext (Var v)          = v
showProdContext (D exp var)      = showD (exp, var)

showPowContextLeft :: Exp -> String
showPowContextLeft (RExp const)
        | const < 0 = addParens . showNum $ const
        | otherwise = showNum const
showPowContextLeft (Sum lexp rexp)  = addParens . showSum $ [lexp, rexp]
showPowContextLeft (Prod lexp rexp) = addParens . showProd $ [lexp, rexp]
showPowContextLeft (Pow base expo)  = addParens . showPow $ (base, expo)
showPowContextLeft (Var v)          = v
showPowContextLeft (D exp var)      = showD (exp, var)

showPowContextRight :: Exp -> String
showPowContextRight (RExp const)     = showNum const
showPowContextRight (Sum lexp rexp)  = addParens . showSum $ [lexp, rexp]
showPowContextRight (Prod lexp rexp) = addParens . showProd $ [lexp, rexp]
showPowContextRight (Pow base expo)  = showPow (base, expo)
showPowContextRight (Var v)          = v
showPowContextRight (D exp var)      = showD (exp, var)

simp3 :: Exp -> Exp
simp3 (Sum (RExp n) (RExp m))                   = RExp (n+m)
simp3 (Sum (RExp n) v)                          = (Sum v (RExp n))
simp3 (Sum u (RExp 0))                          = u
simp3 (Sum (Sum u (RExp n)) (RExp m))           = Sum u (RExp (n+m))
simp3 (Sum (Sum u (RExp n)) v)                  = Sum (Sum u v) (RExp n)
simp3 (Sum u (Sum v w))                         = Sum (Sum u v) w
simp3 (Sum u v)
    | u == v                                    = Prod (RExp 2) u
simp3 (Sum (Prod (RExp n) u) v)
    | u == v                                    = Prod (RExp (n+1)) u
simp3 (Sum u (Prod (RExp n) v))
    | u == v                                    = Prod (RExp (n+1)) u
simp3 (Sum (Prod (RExp n) u) (Prod (RExp m) v))
    | u == v                                    = Prod (RExp (n+m)) u
simp3 (Prod (RExp n) (RExp m))                  = RExp (n*m)
simp3 (Prod u (RExp n))                         = Prod (RExp n) u
simp3 (Prod (RExp 0) v)                         = RExp 0
simp3 (Prod (RExp 1) v)                         = v
simp3 (Prod (RExp n) (Prod (RExp m) v))         = Prod (RExp (n*m)) v
simp3 (Prod u (Prod (RExp n) v))                = Prod (RExp n) (Prod u v)
simp3 (Prod (Prod u v) w)                       = Prod u (Prod v w)
simp3 (Prod (RExp n) (Sum u v))                 = Sum (Prod (RExp n) u) (Prod (RExp n) v)
simp3 (Pow u (RExp 0))                          = RExp 1
simp3 (Pow u (RExp 1))                          = u
simp3 (D (RExp _) _)                            = RExp 0
simp3 (D (Var y)  x)
    | x == y                                    = RExp 1
    | otherwise                                 = RExp 0
simp3 (D (Sum u v) x)                           = Sum (D u x) (D v x)
simp3 (D (Prod u v) x)                          = Sum (Prod u (D v x)) (Prod v (D u x))
simp3 (D (Pow u (RExp n)) x)                    = Prod (Prod (RExp n) (Pow u (RExp (n - 1)))) (D u x)
simp3 u                                         = u

simplify3 :: Exp -> Exp
simplify3 = (visitUntilUnchanged simp3)

visitOnce :: (Exp -> Exp) -> Exp -> Exp
visitOnce f e@(RExp n) = e
visitOnce f e@(Var x)  = e
visitOnce f (Sum u v)  = f (Sum (visitOnce f u) (visitOnce  f v))
visitOnce f (Prod u v) = f (Prod (visitOnce f u) (visitOnce f v))
visitOnce f (Pow u v)  = f (Pow (visitOnce f u) (visitOnce f v))
visitOnce f (D exp x)  = f (D (visitOnce f exp) x)

visitUntilUnchanged :: (Exp -> Exp) -> Exp -> Exp
visitUntilUnchanged f tr = let new = visitOnce f tr
                           in if new == tr
                              then tr
                              else visitUntilUnchanged f new
