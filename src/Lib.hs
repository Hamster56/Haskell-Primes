module Lib
    ( seive
    ,smallPrimes
    ) where

seive :: (Integral a) => [a] -> [a]
seive (p:xs) = p : seive [x | x <- xs, x `mod` p /= 0]

smallPrimes :: (Integral a) => Int -> [a]
smallPrimes n
  | n < 1 = []
  | otherwise = take n $ seive [2..]
