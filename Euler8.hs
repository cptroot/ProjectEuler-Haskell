byFives (x:xs) = if length (x:xs) > 5 then (take 5 (x:xs)) : byFives xs else []

main = do
    input <- readFile "Input8.txt"
    let values = filter (\c -> c /= '\n') input
    let eachValue = byFives values
    print $ maximum $ map (\l -> foldl (\a b -> a * read [b]) 1 l) eachValue