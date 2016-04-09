module LinEqSysRepl where

import Parser4 (parseString, unwrapSeq)
import LinEqSys (solveSystem)
import Eqn (system)

readDoubles :: String -> String -> IO [Double]
readDoubles prompt sentinel = do
        putStr $ prompt ++ ": "

        input <- getLine

        if input == sentinel
            then return []
            else do
                doubles <- readDoubles prompt sentinel

                return $ (read input):doubles

-- interface :: IO ()
-- interface = do
--         putStrLn "Enter some numbers."
--         putStrLn "When finished type 'done'."

--         nums <- readDoubles "Enter a number" "done"

--         putStrLn $ "The average is " ++ (show $ average nums)
--         putStrLn $ "The max is "     ++ (show $ maximum nums)
--         putStrLn $ "The min is "     ++ (show $ minimum nums)

processSystem :: String -> IO [Rational]
processSystem = return . solveSystem . system . unwrapSeq . parseString

pS = return . parseString
uS = return . unwrapSeq
sys = return . system
ss = return . solveSystem

main :: IO ()
main = do
    sysStr <- getLine
    -- result <- processSystem sysStr
    pSr <- pS sysStr
    putStrLn $ show pSr
    uSr <- uS pSr
    putStrLn $ show uSr
    sysr <- sys uSr
    putStrLn $ show sysr
    result <- ss sysr
    putStrLn (show result)
