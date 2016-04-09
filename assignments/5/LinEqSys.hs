module LinEqSys where

sub :: Num a => [a] -> [a] -> [a]
sub = zipWith (-)

scaleList :: Num a => a -> [a] -> [a]
scaleList c = map (*c)

subScale :: Fractional a => [a] -> [a] -> [a]
subScale xs@(x:_) ys@(y:_) = tail $ sub ys (scaleList factor xs)
  where
    factor = y / x

nonZeroFirst :: (Eq a, Fractional a) => [[a]] -> [[a]]
nonZeroFirst xxs = case foldl partitionOnZero ([], []) xxs of
                        ([], _)     -> error ""
                        (ini, rest) -> (reverse ini) ++ rest
  where
    partitionOnZero (ini, rest) list@(0:_) = (ini, list:rest)
    partitionOnZero (ini, rest) list = (list:ini, rest)

dot :: Num a => [a] -> [a] -> a
dot = (sum .) . zipWith (*)

triangulate :: (Eq a, Fractional a) => [[a]] -> [[a]]
triangulate (fixed:xxs) =
        filter (not . null) . map (dropWhile (==0)) $ nonZeroFirst $ fixed:(triangulate' xxs)
    where
          triangulate' []       = []
          triangulate' (ys:yys) = (reduce ys):(triangulate' yys)
          reduce = subScale fixed

solveLine :: Fractional a => [a] -> [a] -> a
solveLine line@(first:rest) subs = ((last line) - (dot rest subs)) / first

solveTriangular :: Fractional a => [[a]] -> [a]
solveTriangular = foldr (\xs subs -> (solveLine xs subs):subs) []

solveSystem :: (Eq a, Fractional a) => [[a]] -> [a]
solveSystem = solveTriangular . triangulate
