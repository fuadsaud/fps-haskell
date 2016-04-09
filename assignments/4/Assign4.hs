import Data.List (genericLength)

newtype K r a = K ((a -> r) -> r)

unWrap :: K r a -> ((a -> r) -> r)
unWrap (K f) = f

instance Monad (K r) where
  return v = K (\k -> k v)
  m >>= f  = K (\k -> (unWrap m) (\v -> unWrap (f v) k))

abortWith :: (Maybe Integer) -> K (Maybe Integer) (Maybe Integer)
abortWith v = K (\k -> v)


index_a :: Eq a => a -> [a] -> (Maybe Integer -> Maybe Integer) -> Maybe Integer
index_a x lst k = index' x (zip [0..] lst) k
  where
      index' x [] k                  = k Nothing
      index' x (y:ys) k | snd y == x = k (Just (fst y))
      index' x (y:ys) k              = index' x ys (\v -> k v)

index_b :: Eq a => a -> [a] -> K (Maybe Integer) (Maybe Integer)
index_b x lst = index' x (zip [0..] lst)
  where
      index' x []                  = abortWith Nothing
      index' x (y:ys) | snd y == x = return (Just (fst y))
      index' x (y:ys)              = index' x ys

index_c :: Eq a => a -> [a] -> Maybe Int
index_c x lst = return 0 >>= index' x lst
  where
     index' x [] _               = fail ""
     index' x (y:ys) i | y == x  = return i
     index' x (y:ys) i           = return (i + 1) >>= (index' x ys)

index_d :: Eq a => a -> [a] -> Maybe Int
index_d x lst = index' x lst 0
  where
     index' x [] _               = fail ""
     index' x (y:ys) i | y == x  = return i
     index' x (y:ys) i           = do
         i' <- return (i + 1)
         index' x ys i'

meetAndGreet :: IO ()
meetAndGreet = do
        putStr "What is your name? "
        name <- getLine
        putStrLn $ "Hello " ++ name ++ "!"

average :: [Double] -> Double
average xs = sum xs / genericLength xs

readDoubles :: String -> String -> IO [Double]
readDoubles prompt sentinel = do
        putStr $ prompt ++ ": "

        input <- getLine

        if input == sentinel
            then return []
            else do
                doubles <- readDoubles prompt sentinel

                return $ (read input):doubles

interface :: IO ()
interface = do
        putStrLn "Enter some numbers."
        putStrLn "When finished type 'done'."

        nums <- readDoubles "Enter a number" "done"

        putStrLn $ "The average is " ++ (show $ average nums)
        putStrLn $ "The max is "     ++ (show $ maximum nums)
        putStrLn $ "The min is "     ++ (show $ minimum nums)

cp :: FilePath -> FilePath -> IO ()
cp f1 f2 = readFile f1 >>= writeFile f2
