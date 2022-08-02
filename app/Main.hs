module Main where

import Lib

main :: IO ()
main = do
    putStrLn $ show $ smallPrimes 50
