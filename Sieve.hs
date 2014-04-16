module Sieve where

import Data.List.Ordered

sieve :: [Int]
sieve = 2:(eratos [3, 5..])
  where
    eratos (x:xs) = x:(eratos [y | y <- xs, y `rem` x /= 0])

sieveMax :: Int -> [Int]
sieveMax m = 2:(eratos [3, 5..m] m)
  where
    eratos (x:xs) m = if x * x > m then x:xs else x: eratos [y | y <- xs, y `rem` x /= 0] m
