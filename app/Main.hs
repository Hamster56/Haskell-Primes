module Main where

import Lib
import System.Environment

main :: IO ()
main = do
    putStrLn $ show $ numPrimes [2..250001]

ps :: (Integral a) => [a]
ps = primesTo $ isqrt 250001

numPrimes :: (Integral a) => [a] -> a
numPrimes xs = foldl (+) 0 $ map f $ map (checkSmallList ps) xs
    where f b
            | b == True  = 1 
            | b == False = 0
