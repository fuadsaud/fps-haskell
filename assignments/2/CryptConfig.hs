module CryptSolve where
  import Data.List
  import Solver

data CryptConfig = CryptConfig [(Char, Integer)] [Char] [Integer] deriving Eq

instance Show CryptConfig where
    show (CryptConfig env _ _) = show env

instance Config CryptConfig where
    successors (CryptConfig env [] _) = []
    successors (CryptConfig env (var:vars) digits) =
      [(CryptConfig ((var, d):env) vars digits') | d <- digits,
      let digits' = [d' | d' <- digits, d' /= d]]

mkIsGoal :: ([Char],[Char],[Char]) -> CryptConfig -> Bool
mkIsGoal (a@(a1:as),b@(b1:bs), c) (CryptConfig env [] _) = (value env a1) > 0 &&
(value env b1) > 0 &&
(satisfy (a, b, c) env)
mkIsGoal eqn _                                           = False

value :: [(Char, Integer)] -> Char -> Integer
value env c = case lookup c env of
                Nothing -> error ("undefined variable: " ++ (show c))
                Just n  -> n

intFromDigits :: [Integer] -> Integer
intFromDigits = foldl (\a d->10*a+d) 0

satisfy :: ([Char],[Char],[Char]) -> [(Char, Integer)] -> Bool
satisfy (a, b, c) env =
    let value' = (value env)
        in ((intFromDigits [value' ch | ch <- a]) +
        (intFromDigits [value' ch | ch <- b])) ==
        (intFromDigits [value' ch | ch <- c])

cryptSolve :: ([Char],[Char],[Char]) -> (Maybe CryptConfig)
cryptSolve eqn@(a,b,c) =
    let sigma      = nub (a++b++c)
        isGoal     = (mkIsGoal eqn)
        initConfig = (CryptConfig [] sigma [0..9])
        in solve isGoal initConfig
