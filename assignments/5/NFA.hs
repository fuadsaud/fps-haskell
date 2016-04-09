module NFA where

import Data.List (union, intersect)

data Str a = EStr | ConCat (Str a) a

stringFromStr :: Show a => Str a -> String
stringFromStr s =
   let helper EStr a         = a
       helper (ConCat s c) a = helper s (show c ++ a)
   in helper s ""

instance Show a => Show (Str a) where
  show s = "<" ++ (stringFromStr s) ++ ">"

unionAll :: Eq a => [[a]] -> [a]
unionAll = foldl union []

data RegExp sigma = RegEmpty                             |
                    RegEpsilon                           |
                    RegSym sigma                         |
                    RegOr (RegExp sigma) (RegExp sigma)  |
                    RegSeq (RegExp sigma) (RegExp sigma) |
                    RegStar (RegExp sigma) deriving (Eq, Show)

data NFA q sigma = NFA (q -> (Maybe sigma) -> [q]) q [q]

instance (Show q, Show sigma) => Show (NFA q sigma) where
        show (NFA _ s f) = "NFA " ++ show s ++ " " ++ show f

epsilonClosure :: Eq q => NFA q sigma -> [q] -> [q]
epsilonClosure (NFA delta _ _) s = epsilonClosure' s
  where
      epsilonClosure' s' =
          let closure = s' `union` foldl union [] [delta state Nothing | state <- s' ]
          in
              if closure == s'
                  then closure
                  else epsilonClosure' closure

deltaStar :: Eq q => NFA q sigma -> ((q, (Str sigma)) -> [q])
deltaStar nfa@(NFA delta _ _) = deltaStar'
  where
      deltaStar' (q, EStr)         = epsilonClosure nfa [q]
      deltaStar' (q, (ConCat x c)) =
          epsilonClosure nfa (unionAll [ delta r (Just c) | r <- deltaStar' (q, x) ])

doesAccept :: Eq q => NFA q sigma -> [sigma] -> Bool
doesAccept nfa@(NFA delta start f) x = not . null $ (deltaStar nfa (start, (list2str x))) `intersect` f
  where
      list2str []     = EStr
      list2str (x:xs) = ConCat (list2str xs) x

nfaFromRegExp :: (Eq sigma) => RegExp sigma -> NFA Int sigma
nfaFromRegExp r = let (_, nfa) = nfaFromRegExp' (-1) r in nfa

nfaFromRegExp' :: (Eq sigma) => Int -> RegExp sigma -> (Int, NFA Int sigma)
nfaFromRegExp' counter RegEmpty = (final, NFA delta start [final])
  where
      start = counter + 1
      final = start + 1
      delta _ _ = []

nfaFromRegExp' counter RegEpsilon = (final, NFA delta start [final])
  where
      start           = counter + 1
      final           = start
      delta _ Nothing = [start]
      delta _ _       = []

nfaFromRegExp' counter (RegSym sym) = (final, NFA delta start [final])
  where
      start                        = counter + 1
      final                        = start   + 1
      delta q (Just x)
          | q == start && x == sym = [final]
      delta _ _                    = []

nfaFromRegExp' counter (RegOr r1 r2) = (final, NFA delta start [final])
  where
      start                    = counter + 1
      (c1, (NFA d1 _ _))       = nfaFromRegExp' start r1
      (c2, (NFA d2 _ _))       = nfaFromRegExp' c1 r2
      final                    = c2 + 1
      delta q Nothing
          | q == start         = [start + 1, c1 + 1]
          | q == c1 || q == c2 = [final]
      delta q x                = (d1 q x) `union` (d2 q x)

nfaFromRegExp' counter (RegSeq r1 r2) = (final, NFA delta start [final])
  where
      start                      = counter + 1
      (c1, (NFA delta1 _ f1))    = nfaFromRegExp' counter r2
      (final, (NFA delta2 _ f2)) = nfaFromRegExp' c1 r1
      delta q Nothing
          | q == c1              = [c1 + 1]
      delta q x
          | q == start           = delta1 q x
          | q == c1 + 1          = delta2 q x
      delta _ _                  = []

nfaFromRegExp' counter (RegStar r) = (final, NFA delta start [final])
  where
      start                          = counter + 1
      (c, (NFA d _ f))               = nfaFromRegExp' start r
      final                          = c + 1
      delta q Nothing
          | q == start || q `elem` f = [start + 1, final]
      delta q x                      = d q x
