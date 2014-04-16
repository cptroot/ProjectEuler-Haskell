import Sieve
import Divisor

triangles = [n * (n + 1) `div` 2 | n <- [1..]]
bigEnough = sieveMax 1000000

firstHugeDivisor = head [n | n <- triangles, let divisors = numDivisors . factorPowers $ factorsWith n bigEnough, divisors > 500]

main = print $ firstHugeDivisor