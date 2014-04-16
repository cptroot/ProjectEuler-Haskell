module Divisor where

import Data.List

import Sieve

factorsWith :: Int -> [Int] -> [Int]
factorsWith n (x:xs) = if n == 1 then [] else (if (n `rem` x) == 0 then x : factorsWith (n `div` x) (x: xs) else factorsWith n xs)

factors :: Int -> [Int]
factors n = factorsWith n $ sieveMax (n `div` 2)

factorPowers :: [Int] -> [(Int, Int)]
factorPowers l = [(p, length $ elemIndices p l) | p <- nub l]

numDivisors :: [(Int, Int)] -> Int
numDivisors [] = 1
numDivisors ((p, n):xs) = (n + 1) * numDivisors xs