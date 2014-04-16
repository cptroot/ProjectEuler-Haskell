
collatz :: Integral a => a -> a
collatz n 
  | (n `rem` 2 == 0) = n `div` 2
  | otherwise = 3 * n + 1

collatzLength :: Integral a => a -> Int
collatzLength 1 = 1
collatzLength n = 1 + (collatzLength $ collatz n)

memoizedCollatzLength :: Int -> Int
memoizedCollatzLength = (map tempCollatzLength [0..] !!)
  where tempCollatzLength 1 = 1
        tempCollatzLength n = 1 + (memoizedCollatzLength (collatz n))

main = print . maximum $ map memoizedCollatzLength ([1..10000] :: [Int])