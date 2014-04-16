
filter35 l = filter (\x -> rem x 3 == 0 || rem x 5 == 0) l

main = print (foldl (+) 0 (filter35 [1..(1000-1)]))