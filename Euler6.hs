

main = print $ (foldl1 (+) [1..100]) ^ 2 - (foldl1 (+) (map (^2) [1..100])) 