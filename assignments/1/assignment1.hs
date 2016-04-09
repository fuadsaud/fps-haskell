second :: [a] -> a
second lst = head (tail lst)
-- second _:x:xs = x

singleton :: [a] -> Bool
singleton []                = False
singleton (x:xs) | null xs  = True
singleton (x:xs)            = False

index :: Eq a => a -> [a] -> Maybe Integer
index x lst = index' x (zip [0..] lst)
  where
    index' x []                  = Nothing
    index' x (y:ys) | snd y == x = Just (fst y)
    index' x (y:ys)              = index' x ys

evenSquares' :: [Integer] -> [Integer]
evenSquares' lst = map (^ 2) (filter even lst)

insert :: Ord a => a -> [a] -> [a]
insert x []             = [x]
insert x (y:ys) | x < y = x:y:ys
insert x (y:ys)         = y:(insert x ys)

insertionSort :: Ord a => [a] -> [a]
insertionSort []     = []
insertionSort (x:xs) = insert x (insertionSort xs)

insertionSortH :: Ord a => [a] -> [a]
insertionSortH lst = foldr insert [] lst

perm :: [a] -> [[a]]
perm []  = [[]]
perm lst = [ y:ys | (y,xs) <- select lst, ys <- perm xs ]
  where
    select [] = []
    select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

data Peano = Zero | S Peano deriving Show

add :: Peano -> Peano -> Peano
add Zero b  = b
add (S a) b = S (add a b)

mult :: Peano -> Peano -> Peano
mult Zero b     = Zero
mult (S Zero) b = b
mult (S a) b    = add (mult a b) b

fact :: Peano -> Peano
fact Zero  = (S Zero)
fact (S a) = mult (S a) (fact a)

meaning :: Peano -> (a -> a)
meaning Zero  = (\z -> z)
-- meaning s@(S a) = (\z -> (s ((meaning a) s z)))

fromPeano :: Peano -> Integer
fromPeano Zero  = (meaning Zero) 0
fromPeano (S a) = succ (fromPeano a)
