-- Spigot algorithm reported in Jeremy Gibbon's article
-- in _The American Mathematical Monthly_ Vol 113, No 4, April 2006.

g q r t i = let (u, y) = (3*(3*i+1)*(3*i+2), (q*(27*i-12)+5*r) `div` (5*t))
             in y:(g (10*q*i*(2*i-1)) (10*u*(q*(5*i-2)+r-y*t)) (t*u) (i+1))

piSpigot = g 1 180 60 2
