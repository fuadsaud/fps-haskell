-- Author: Arthur Nunes-Harwitt

module PolyMath where
import Data.Ratio
import SymMath


-- R[x] is defined recursively as follows.
-- * a in R
-- * p*x+a in R[x] if p in R[x] and a in R
--   for canonical form, p should not be 0

data Poly = Const Rational | ProdPlus Poly String Rational deriving Eq

instance Show Poly where
  show p =
    let show' n x (Const a) = showTerm a x n
        show' n x (ProdPlus p y a)
          | a > 0     = (show' (n+1) y p)++"+"++(showTerm a x n)
          | a < 0     = (show' (n+1) y p)++"-"++(showTerm (abs a) x n)
          | otherwise = (show' (n+1) y p)
        showTerm a x 0 = showNum a
        showTerm 1 x n = showPow x n
        showTerm a x n = (showNum a)++"*"++(showPow x n)
        showPow x 1 = x
        showPow x n = x ++ "^" ++ (show n)
        showNum a
          | denominator a == 1     = show (numerator a)
          | otherwise              =
              "("++(show (numerator a))++"/"++(show (denominator a))++")"
    in show' 0 "" p

fromConst :: Rational -> Poly
fromConst a = Const a

fromVar :: String -> Poly
fromVar x = (ProdPlus (Const 1) x 0)

addConstOverVar :: Rational -> String -> Poly -> Poly
addConstOverVar a x (Const b) = Const (a+b)
addConstOverVar a x (ProdPlus p y b)
  | x == y                    = ProdPlus p x (a+b)

scale :: Rational -> Poly -> Poly
scale 0 p                = Const 0
scale a (Const b)        = Const (a*b)
scale a (ProdPlus p x b) = ProdPlus (scale a p) x (a*b)

mulVar :: String -> Poly -> Poly     -- multiply by x
mulVar x (Const 0)                   = (Const 0)
mulVar x p@(Const a)                 = (ProdPlus p x 0)
mulVar x p@(ProdPlus _ y _) | x == y = (ProdPlus p x 0)

normalPoly :: Poly -> String -> Rational -> Poly
normalPoly (Const 0) x a = Const a
normalPoly p x a         = ProdPlus p x a

instance Num Poly where
  (Const a) + (Const a')        = Const (a+a')
  (ProdPlus p x a) + (Const a') = (ProdPlus p x (a+a'))
  (Const a) + (ProdPlus p x a') = (ProdPlus p x (a+a'))
  (ProdPlus p x a) + (ProdPlus p' x' a')
     | x == x'                  = normalPoly (p+p') x (a+a')

  negate p = scale (-1) p

  (Const a) * p = scale a p
  (ProdPlus p x a) * p' = (p * (x `mulVar` p')) + (scale a p')

  abs _ = error "abs not supported for type Poly"
  signum _ = error "signum not supported for type Poly"
  fromInteger  = fromConst . fromInteger

fromExp :: Exp -> Poly
fromExp (IExp n) = fromInteger n
fromExp (Var x) = fromVar x
fromExp (Sum u v) = (fromExp u) + (fromExp v)
fromExp (Prod u v) = (fromExp u) * (fromExp v)
fromExp (Pow u v) = let n = intEval v
                    in if n >=0 then (fromExp u) ^ n
                       else error "Fractional polynomial encountered"

derivative :: Poly -> String -> Poly
derivative (Const _) x = Const 0
derivative (ProdPlus p x a) y
  | x == y             =  p + (mulVar x (derivative p x))
  | otherwise          = Const 0

integral :: Poly -> String -> Poly
integral (Const 0) x                         = Const 0
integral p y =
    let intCount (Const a) n                 = Const (a/(n+1))
        intCount (ProdPlus p x a) n | x == y = ProdPlus (intCount p (n+1)) x (a/(n+1))
    in  ProdPlus (intCount p 0) y 0
