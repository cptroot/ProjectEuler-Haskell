import Data.Text (Text, split, pack, unpack)
import Data.Array
-- import Data.Array.Unboxed

indices a length = map (\b -> (a, b)) [1..length]

parseLine :: Text -> [Int]
parseLine line = map (read . unpack) (split (== ' ') line)

multHoriz (x, y) arr = (arr ! (x, y)) *
  (arr ! (x + 1, y)) *
  (arr ! (x + 2, y)) *
  (arr ! (x + 3, y))

multVert (x, y) arr = (arr ! (x, y)) *
  (arr ! (x, y + 1)) *
  (arr ! (x, y + 2)) *
  (arr ! (x, y + 3))

multDiagRight (x, y) arr = (arr ! (x, y)) *
  (arr ! (x + 1, y + 1)) *
  (arr ! (x + 2, y + 2)) *
  (arr ! (x + 3, y + 3))

multDiagLeft (x, y) arr = (arr ! (x, y)) * 
  (arr ! (x - 1, y + 1)) *
  (arr ! (x - 2, y + 2)) *
  (arr ! (x - 3, y + 3))

main = do
  input <- readFile "Input11.txt"
  let lines = filter (\s -> s /= pack "") . split (== '\n') $ pack input
  let parsedLines = map parseLine lines
  let indexList = map (\a -> Main.indices a $ length . head $ parsedLines) [1..length parsedLines]
  let linesIndexes = concat $ zipWith (\a b -> zip a b) indexList parsedLines
  let intArray = array ((1, 1), (length lines, length lines)) linesIndexes
  let horizMax = maximum [multHoriz (x, y) intArray | x <- [1..length lines - 3], y <- [1..length lines]]
  let vertMax = maximum [multVert (x, y) intArray | x <- [1..length lines], y <- [1..length lines - 3]]
  let diagRightMax = maximum [multDiagRight (x, y) intArray | x <- [1..length lines - 3], y <- [1..length lines - 3]]
  let diagLeftMax = maximum [multDiagLeft (x, y) intArray | x <- [4..length lines], y <- [1..length lines - 3]]
  print $ maximum [horizMax, vertMax, diagRightMax, diagLeftMax]
  -- let matrix = listArray (1, length lines) (map (\line -> parseLine line) lines)
  -- print matrix