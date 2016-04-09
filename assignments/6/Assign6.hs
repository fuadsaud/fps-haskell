data Const = IConst Int                              |
             BConst Bool                             |
             FConst1 String (Const -> Const)         |
             FConst2 String (Const -> Const -> Const)

instance Eq Const where
    (IConst x) == (IConst y)     = x == y
    (BConst a) == (BConst b)     = a == b
    (FConst1 n1 _) == (FConst1 n2 _) = n1 == n2
    (FConst2 n1 _) == (FConst2 n2 _) = n2 == n2

instance Show Const where
    show (IConst x) = show x
    show (BConst a) = show a
    show (FConst1 n _) = "<prim1:" ++ n ++ ">"
    show (FConst2 n _) = "<prim2:" ++ n ++ ">"

data Exp = Econ Const        |
           Var String        |
           Lambda String Exp |
           IfExp Exp Exp Exp |
           Appl Exp Exp      deriving (Eq, Show)

data Op =  S   |
           K   |
           I   |
           B   |
           C   |
           CIf deriving (Eq, Show)

data CExp = Ccon Const      |
            CVar String     |
            Cop Op          |
            CAppl CExp CExp deriving (Eq, Show)

type Env = [(String, Const)]

primAbs :: Const
primAbs = FConst1 "abs" f
  where
      f (IConst i) = IConst (abs i)

primPlus :: Const
primPlus = FConst2 "+" f
  where
      f (IConst i) (IConst j) = IConst (i + j)

primSub :: Const
primSub = FConst2 "-" f
  where
      f (IConst i) (IConst j) = IConst (i - j)

primMult :: Const
primMult = FConst2 "*" f
  where
      f (IConst i) (IConst j) = IConst (i * j)

primDiv :: Const
primDiv = FConst2 "div" f
  where
      f (IConst i) (IConst j) = IConst (i `div` j)

primEq :: Const
primEq = FConst2 "==" f
  where
      f (IConst i) (IConst j) = BConst (i == j)

initEnv :: Env
initEnv = [("abs", primAbs),
           ("+", primPlus),
           ("-", primSub),
           ("*", primMult),
           ("div", primDiv),
           ("==", primEq)]

compile :: Exp -> CExp
compile (Econ const)  = Ccon const
compile (Var s)       = CVar s
compile (IfExp a b c) = CAppl (CAppl (CAppl (Cop CIf) (compile a)) (compile b)) (compile c)
compile (Lambda arg (Var s))
    | arg == s = Cop I

compile (Lambda arg (Appl (Var f) (Var x)))
    | x == arg = case (lookup f initEnv) of
                     Nothing -> error ("can't find function `" ++ f ++ "`")
                     Just ff -> Ccon ff

compile (Lambda arg body) = abstract arg (compile body) initEnv
-- compile (Lambda arg (Appl f x)) = CAppl (compile f) (compile x)

compile (Appl f x) = CAppl (compile f) (compile x)

abstract :: String -> CExp -> Env -> CExp
abstract _ c@(Ccon _) rho   = CAppl (Cop K) c
abstract x (CVar y) rho
    | x == y                = Cop I
    | x /= y                = CAppl (Cop K) (case (lookup y rho) of
                                                 Nothing -> CVar y
                                                 Just yy -> Ccon yy)
abstract _ op@(Cop _) rho   = CAppl (Cop K) op
abstract x appl@(CAppl m n) rho
    | am == km && an == (Cop I)                      = m
    | am == km && an == kn                           = (CAppl (Cop K) appl)
    | am == km && an == n && n /= (Cop I) && n /= kn = CAppl (CAppl (Cop B) m) n
    | am == m && am /= km && an == kn                = CAppl (CAppl (Cop C) m) n
    | am == m && am /= km && an == n && an /= kn     = CAppl (CAppl (Cop C) m) n
  where
      am = abstract x m rho
      an = abstract x n rho
      km = CAppl (Cop K) m
      kn = CAppl (Cop K) n

reduceComb :: CExp -> CExp
reduceComb (CAppl (Cop I) exp) = exp
reduceComb (CAppl (CAppl (Cop K) exp ) _) = exp
reduceComb (CAppl (CAppl (CAppl (Cop S) f) g) x) = CAppl (CAppl f x) (CAppl g x)
reduceComb (CAppl (CAppl (CAppl (Cop B) f) g) x) = CAppl f (CAppl g x)
reduceComb (CAppl (CAppl (CAppl (Cop C) f) x) y) = CAppl (CAppl f x) y
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst True))) exp) _)  = exp
reduceComb (CAppl (CAppl (CAppl (Cop CIf) (Ccon (BConst False))) _) exp) = exp
reduceComb (CAppl (Ccon (FConst1 _ f)) (Ccon c)) = Ccon (f c)
reduceComb (CAppl (CAppl (Ccon (FConst2 _ f)) (Ccon c)) (Ccon d)) = Ccon ((f c) d)
reduceComb exp = exp

run :: CExp -> CExp
run = converge reduceComb
  where
      converge = until =<< ((==) =<<)

compileAndRun :: Exp -> CExp
compileAndRun = run . compile
