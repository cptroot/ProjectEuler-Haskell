import Data.Text (Text, split, pack, unpack)

readInt :: String -> Integer
readInt = read

main = do
  input <- readFile "Input13.txt"
  let lines = filter (\s -> s /= pack "") . split (== '\n') $ pack input
  let numbers = map (readInt . unpack) lines
  let sum = foldl1 (+) numbers
  print (readInt $ take 10 $ show sum)