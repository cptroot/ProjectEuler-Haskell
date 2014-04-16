

isPalindromeNum n = isPalindrome $ show n

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = (reverse $ take (length l `div` 2) l) == (drop ((length l + 1) `div` 2) l)

main = print . maximum . filter isPalindromeNum $ concat $ map (\x -> map (\y -> y * x) [x..999]) [100..999]