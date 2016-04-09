module SymMath where


data Exp = IExp Integer   |
           Var String     |
           Sum Exp Exp    |
           Prod Exp Exp   |
           Pow Exp Exp  deriving Eq

instance Show Exp where
  show (IExp n)
    | n < 0           = "(" ++ (show n) ++ ")"
    | otherwise       = show n
  show (Var x)        = x
  show (Sum u v)      = "("++(show u)++"+"++(show v)++")"
  show (Prod u v)     = "("++(show u)++"*"++(show v)++")"
  show (Pow u v)      = "("++(show u)++"^"++(show v)++")"


intEval :: Exp -> Integer
intEval (IExp n) = n
intEval (Var x) = error "Variable encountered in integer expression"
intEval (Sum u v) = (intEval u) + (intEval v)
intEval (Prod u v) = (intEval u) * (intEval v)
intEval (Pow u v) = let nv = intEval v
                    in if nv >= 0 then
                          (intEval u) ^ nv
                       else error "Fraction encountered in integer expression"

deriv :: Exp -> String -> Exp
deriv (IExp n) x = IExp 0
deriv (Var x) y
  | x == y    = IExp 1
  | otherwise = IExp 0
deriv (Sum u v) x  = Sum (deriv u x) (deriv v x)
deriv (Prod u v) x = Sum (Prod u (deriv v x)) (Prod v (deriv u x))
deriv (Pow u (IExp n)) x = (Prod (Prod (IExp n) (Pow u (IExp (n-1))))
                                 (deriv u x))

visitOnce :: (Exp -> Exp) -> Exp -> Exp
visitOnce f e@(IExp n) = e
visitOnce f e@(Var x)  = e
visitOnce f (Sum u v)  = f (Sum (visitOnce f u) (visitOnce  f v))
visitOnce f (Prod u v) = f (Prod (visitOnce f u) (visitOnce f v))
visitOnce f (Pow u v)  = f (Pow (visitOnce f u) (visitOnce f v))


simp :: Exp -> Exp
simp (Sum (IExp n) (IExp m))  = IExp (n+m)
simp (Sum (IExp 0) v)         = v
simp (Sum u (IExp 0))         = u
simp (Prod (IExp n) (IExp m)) = IExp (n*m)
simp (Prod (IExp 0) v)        = IExp 0
simp (Prod u (IExp 0))        = IExp 0
simp (Prod (IExp 1) v)        = v
simp (Prod u (IExp 1))        = u
simp (Pow u (IExp 0))         = IExp 1
simp (Pow u (IExp 1))         = u
simp u                        = u

simplify1 :: Exp -> Exp
simplify1  = (visitOnce simp)

visitUntilUnchanged :: (Exp -> Exp) -> Exp -> Exp
visitUntilUnchanged f tr = let new = visitOnce f tr
                           in if new == tr
                              then tr
                              else visitUntilUnchanged f new

simp2 :: Exp -> Exp
simp2 (Sum (IExp n) (IExp m))           = IExp (n+m)
simp2 (Sum (IExp n) v)                  = (Sum v (IExp n))
simp2 (Sum u (IExp 0))                  = u
simp2 (Sum (Sum u (IExp n)) (IExp m))   = Sum u (IExp (n+m))
simp2 (Sum (Sum u (IExp n)) v)          = Sum (Sum u v) (IExp n)
simp2 (Sum u (Sum v w))                 = Sum (Sum u v) w
simp2 (Sum u v)
  | u == v                              = Prod (IExp 2) u
simp2 (Sum (Prod (IExp n) u) v)
  | u == v                              = Prod (IExp (n+1)) u
simp2 (Sum u (Prod (IExp n) v))
  | u == v                              = Prod (IExp (n+1)) u
simp2 (Sum (Prod (IExp n) u) (Prod (IExp m) v))
  | u == v                              = Prod (IExp (n+m)) u
simp2 (Prod (IExp n) (IExp m))          = IExp (n*m)
simp2 (Prod u (IExp n))                 = Prod (IExp n) u
simp2 (Prod (IExp 0) v)                 = IExp 0
simp2 (Prod (IExp 1) v)                 = v
simp2 (Prod (IExp n) (Prod (IExp m) v)) = Prod (IExp (n*m)) v
simp2 (Prod u (Prod (IExp n) v))        = Prod (IExp n) (Prod u v)
simp2 (Prod (Prod u v) w)               = Prod u (Prod v w)
simp2 (Prod (IExp n) (Sum u v))         = Sum (Prod (IExp n) u) (Prod (IExp n) v)
simp2 (Pow u (IExp 0))                  = IExp 1
simp2 (Pow u (IExp 1))                  = u
simp2 u                                 = u

simplify2 :: Exp -> Exp
simplify2 = (visitUntilUnchanged simp2)
