module Lib
    ( checkSmall
    ,smallPrimes
    ) where

import Data.List

seive :: (Integral a) => [a] -> [a]
seive (p:xs) = p : seive [x | x <- xs, x `mod` p /= 0]

smallPrimes :: (Integral a) => Int -> [a]
smallPrimes n
  | n < 1 = []
  | otherwise = take n $ seive [2..]

checkSmall :: (Integral a) => a -> Bool
checkSmall p = not . foldl (||) False $ map (divisible p) (smallPrimes 50)

divisible :: (Integral a) => a -> a -> Bool
divisible a b
  | a == b          = False
  | a `mod` b /= 0  = False
  | otherwise       = True
