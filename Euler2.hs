import Data.List

takemax n (x:xs) = if x > n then [] else x: (takemax n xs)

main = print (foldl1 (+) (filter even (takemax 4000000 fibonacci)))
	where
	fibonacci = unfoldr (\(a, b) -> Just (a + b, (b, a + b))) (1, 1)