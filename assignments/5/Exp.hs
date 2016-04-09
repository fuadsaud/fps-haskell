module Exp where

import Data.Ratio

data Exp = RExp Rational  |
           IExp Integer   |
           Var String     |
           Sum Exp Exp    |
           Diff Exp Exp   |
           Prod Exp Exp   |
           Quo Exp Exp    |
           Neg Exp deriving (Eq, Show)
