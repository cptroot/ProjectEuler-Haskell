slow_fib :: Int -> Integer
slow_fib 0 = 0
slow_fib 1 = 1
slow_fib n = slow_fib (n - 1) + slow_fib (n - 2)

memoized_fib :: Int -> Integer
memoized_fib n = (map fib [0..] !!) n
  where fib 0 = 0
        fib 1 = 1
        fib n = memoized_fib (n - 1) + memoized_fib (n - 2)

main = print $ memoized_fib 60