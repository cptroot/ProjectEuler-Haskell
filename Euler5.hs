
import Data.List

factors :: Int -> [Int]
factors n = factorsGeqThan n 2

factorsGeqThan :: Int -> Int -> [Int]
factorsGeqThan n min = if n == 1 then [] else 
                       if n `rem` min == 0 then min:factorsGeqThan (n `div` min) min else factorsGeqThan n (min + 1)

frequency list = [(foldl (\x a -> if x == y then 1 + a else a) 0 list, y) | y <- nub list]

lcm2 :: [Int] -> Int
lcm2 l = multFactors . combineFactors $ map (frequency . factors) l
    where combineFactors [] = []
          combineFactors (x:xs) = [(max a b, n1) | (a, n2) <- x, (b, n1) <- combineFactors xs, n1 == n2]

multFactors :: [(Int, Int)] -> Int
multFactors [] = 1
multFactors ((n, p):xs) = (n ^ p) * multFactors xs



main = print ( foldl1 lcm [1..20])