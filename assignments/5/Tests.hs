import NFA

import Data.List (elemIndex)

tests = [
    doesAccept (nfaFromRegExp RegEmpty) ""                              == False,
    doesAccept (nfaFromRegExp RegEmpty) "a"                             == False,
    doesAccept (nfaFromRegExp RegEpsilon) ""                            == True,
    doesAccept (nfaFromRegExp RegEpsilon) "a"                           == False,
    doesAccept (nfaFromRegExp RegEpsilon) "b"                           == False,
    doesAccept (nfaFromRegExp (RegSym 'a')) ""                          == False,
    doesAccept (nfaFromRegExp (RegSym 'a')) "a"                         == True,
    doesAccept (nfaFromRegExp (RegSym 'a')) "b"                         == False,
    doesAccept (nfaFromRegExp (RegSym 'a')) "aa"                        == False,
    doesAccept (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) ""     == False,
    doesAccept (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) "a"    == True,
    doesAccept (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) "b"    == True,
    doesAccept (nfaFromRegExp (RegOr (RegSym 'a') (RegSym 'b'))) "ab"   == False,
    doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) ""    == False,
    doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "a"   == False,
    doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "b"   == False,
    doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "ba"  == False,
    doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "ab"  == True,
    doesAccept (nfaFromRegExp (RegSeq (RegSym 'a') (RegSym 'b'))) "aba" == False,
    doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) ""                == True,
    doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) "a"               == True,
    doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) "aa"              == True,
    doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) "b"               == False,
    doesAccept (nfaFromRegExp (RegStar (RegSym 'a'))) "ab"              == False]

main = do
        putStrLn (show tests)
        putStrLn . show . length . filter not $ tests
        putStrLn . show . elemIndex False $ tests
