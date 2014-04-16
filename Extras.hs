module Extras

palindrome :: [a] -> Bool
palindrome = compare $ halves
  where
    compare ((x1:l1), (x2:l2)) = if x1 == x2 then compare (l1, l2) else False
    halves l = pushpop l (div $ length l 2)
    pushpop l n = (