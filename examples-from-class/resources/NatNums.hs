module NatNums where

sieve :: [Integer] -> [Integer]
sieve (n:ns) = n:(sieve [m | m <- ns, m `rem` n /= 0])

primes :: [Integer]
primes = sieve [2..]

add :: [Integer] -> [Integer] -> [Integer]
add (m:ms) (n:ns) = (m+n):(add ms ns)

fibs = 0:1:(add (tail fibs) fibs)
