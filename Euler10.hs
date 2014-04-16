import Sieve

main = print . foldl (+) 0 $ sieveMax 2000000