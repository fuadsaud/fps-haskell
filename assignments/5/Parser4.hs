module Parser4 where

import Exp
import Eqn
import Scanner
import Control.Monad

data Seq = Seq [Eqn] |
           ParseEqnSeqError String deriving Show

unwrapSeq :: Seq -> [Eqn]
unwrapSeq (Seq s) = s

stringFromToken :: Token -> String
stringFromToken (Compound (Id s)) = s

integerFromToken :: Token -> Integer
integerFromToken (Compound (Num n)) = n

rationalFromToken (Compound (Num n)) = toRational n

newtype Parser a = Parser ([Token] -> [(a, [Token])])

unWrap (Parser f) = f

instance Monad Parser where
  return a = Parser(\ts->[(a, ts)])
  p >>= f = Parser(\ts->[(b, ts2) | (a, ts1) <- unWrap p ts, (b, ts2) <- unWrap (f a) ts1])

instance Applicative Parser where
        pure = return
        (<*>) = ap

instance Functor Parser where
        fmap f pa = let (a, _) = head ((unWrap pa) []) in return (f a)

failParser :: Parser a
failParser = Parser(\ts-> [])

-- item makes sense for "token" Parsers
item :: Parser Token
item = Parser(\ts->case ts of [] -> []; t:ts1 -> [(t,ts1)])

parserFilter :: Parser b -> (b -> Bool) -> Parser b
parserFilter parser p = do {a <- parser; if p a then return a else failParser}

literal :: Token -> Parser Token
literal t = parserFilter item (==t)

variable :: Parser Token
variable =  parserFilter item (\tok->case tok of (Compound (Id _)) -> True; _ -> False)

number :: Parser Token
number =  parserFilter item (\tok->case tok of (Compound (Num _)) -> True; _ -> False)

(+++) :: Parser a -> Parser a -> Parser a
p1 +++ p2 = Parser(\ts-> (unWrap p1 ts) ++ (unWrap p2 ts))

getEqnSys :: Parser [Eqn]
getEqnSys =
    (do eqn <- getEqn
        (do (literal (Simple COMMA))
            sys <- getEqnSys
            return (eqn:sys))
        +++
        (do eqn <- getEqn
            return [eqn]))

getEqn :: Parser Eqn
getEqn = do
        lhs <- getMathExp
        (literal (Simple EQ1))
        rhs <- getMathExp
        return (Eqn lhs rhs)

getExp :: Parser Exp
getExp = getMathExp

getMathExp :: Parser Exp
getMathExp =
  do term <- getTerm
     getMathExp' term

getMathExp' :: Exp -> Parser Exp
getMathExp' term =
  (do tok <- (literal (Simple PLUS))
      term2 <- getTerm
      getMathExp' (Sum term term2))
  +++
  (do tok <- (literal (Simple MINUS))
      term2 <- getTerm
      getMathExp' (Diff term term2))
  +++
   (return term)

getTerm :: Parser Exp
getTerm =
  do factor <- getFactor
     getTerm' factor

getTerm' :: Exp -> Parser Exp
getTerm' factor =
  (do tok <- (literal (Simple STAR))
      factor2 <- getFactor
      getTerm' (Prod factor factor2))
  +++
  (do tok <- (literal (Simple SLASH))
      factor2 <- getFactor
      getTerm' (Quo factor factor2))
  +++
   (return factor)

getFactor :: Parser Exp
getFactor =
  (do vtok <- variable
      return (Var (stringFromToken vtok)))
  +++
  (do ntok <- number
      return (RExp (rationalFromToken ntok)))
  +++
  (do tok <- (literal (Simple MINUS))
      factor <- getFactor
      return (Neg factor))
  +++
  (do tok <- (literal (Simple OP))
      exp <- getExp
      tok <- (literal (Simple CP))
      return exp)

parse :: [Token] -> Seq
parse ts =
  case unWrap getEqnSys ts of
    []            -> ParseEqnSeqError "Bad input"
    (eqn, ts1):ps -> if isEmptyTokenStream ts1
                     then Seq eqn
                     else ParseEqnSeqError "Unconsumed input"

parseString :: String -> Seq
parseString = parse . tokenStreamFromString
