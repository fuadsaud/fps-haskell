module SatisfiabilitySolve where

import Solver
import Data.Map (Map)
import qualified Data.Map as Map

data BExp = BConst Bool | Var String | And BExp BExp | Or BExp BExp | Not BExp deriving (Eq)

instance Show BExp where
    show (BConst value)  = show value
    show (Var varName)   = varName
    show (And lexp rexp) = "(" ++ (show lexp) ++ " && " ++ (show rexp) ++ ")"
    show (Or  lexp rexp) = "(" ++ (show lexp) ++ " || " ++ (show rexp) ++ ")"
    show (Not exp)       = '~':(show exp)

data SatConfig = SatConfig BExp (Map String Bool) deriving Eq

instance Show SatConfig where
  show (SatConfig bexp) = show bexp

instance Config SatConfig where
    successors config = []

isSatisfiable :: SatConfig -> Bool
isSatisfiable (SatConfig (BConst True))   = True
isSatisfiable (SatConfig (BConst False))  = False
isSatisfiable (SatConfig (Var _))         = True
isSatisfiable (SatConfig (And lexp rexp)) = isSatisfiable (SatConfig lexp) && isSatisfiable (SatConfig rexp)
isSatisfiable (SatConfig (Or lexp rexp))  = isSatisfiable (SatConfig lexp) || isSatisfiable (SatConfig rexp)
isSatisfiable (SatConfig (Not exp))       = isSatisfiable (SatConfig exp)

satSolve :: BExp -> Maybe SatConfig
satSolve bexp = solve isSatisfiable initialConfig
  where
    initialConfig = SatConfig bexp (Map.fromList [])
