module Solver where

class (Eq c, Show c) => Config c where
    successors :: c -> [c]

solveAll :: (Config c) => (c -> Bool) -> c -> [c]
solveAll isGoal c = let restSolutions = concat [solveAll isGoal c' | c' <- successors c]
                        in if isGoal c then c:restSolutions else restSolutions

solve :: (Config c) => (c -> Bool) -> c -> (Maybe c)
solve isGoal c = case solveAll isGoal c of
                   []   -> Nothing
                   x:xs -> Just x
