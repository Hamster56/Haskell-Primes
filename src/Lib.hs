module Lib
    ( checkSmall
    ,checkSmallList
    ,smallPrimes
    ,divisible
    ,isqrt
    ,primesTo
    ) where

import Data.List

seive :: (Integral a) => [a] -> [a]
seive (p:xs) = p : seive [ x | x <- xs, x `mod` p /= 0 ]

smallPrimes :: (Integral a) => a -> [a]
smallPrimes n
  | n < 1 = []
  | otherwise = genericTake n $ seive [2..]

primesTo :: (Integral a) => a -> [a]
primesTo n
  | n <1 = []
  | otherwise = takeWhile (<= n) $ seive [2..]

checkSmall :: (Integral a) => a -> a -> Bool
checkSmall n p = null [ x | x <- smallPrimes n, p `mod` x == 0, p /= x ]

checkSmallList :: (Integral a) => [a] -> a -> Bool
checkSmallList ns p = null [ x | x <- ns, p `mod` x == 0, p /= x ]

divisible :: (Integral a) => a -> a -> Bool
divisible a b
  | a == b          = False
  | a `mod` b /= 0  = False
  | otherwise       = True

isqrt :: (Integral a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = head $ dropWhile (\x -> x*x > n) $ iterate (\x -> (x + n `div` x ) `div` 2) (n `div` 2)

trialDivision :: (Integral a) => a -> Bool
trialDivision p = if p > 1 then null [ x | x <- [2..isqrt p], p `mod` x == 0] else False
