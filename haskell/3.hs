import Data.Char
import Data.List
import Data.Matrix
import Data.Maybe

-- Returns the 9 coordinates around the given one
getNeighbors :: (Int, Int) -> [(Int, Int)]
getNeighbors (x, y) = [(x + i, y + j) | i <- [-1..1], j <- [-1..1], (i, j) /= (0, 0)]

-- O(n log n) nub
fastNub :: [(Int, Int)] -> [(Int, Int)]
fastNub = map head . group . sort

-- Gives all the coordinates around the supplied ones
allNeighbors :: [(Int, Int)] -> [(Int, Int)]
allNeighbors = fastNub . (getNeighbors =<<)

-- Extracts all nonnumeric non . symbols out of a given char matrix
symbols :: Matrix Char -> String
symbols = nub . filter (not . isDigit) . filter ('.'/=) . toList

-- From a list of Maybe values, accumulate contiguous justs into lists and drop nothings
extractJust :: [Maybe a] -> [[a]]
extractJust [] = []
extractJust (Nothing:xs) = extractJust (dropWhile isNothing xs)
extractJust xs = (catMaybes justs) : extractJust rest
  where
    (justs, rest) = span isJust xs

-- Returns the number represented by the indices in the supplied list in matrix
indicesNum :: Matrix Char -> [(Int, Int)] -> Int
indicesNum m = read . map (m!)

-- Scans the matrix for contiguous numbers and returns their index list
getNums :: Matrix Char -> [[(Int, Int)]]
getNums = extractJust . toList . mapPos (\i v -> if isDigit v then Just i else Nothing)

-- A safe get which takes in a pair instead of two values
safeGet1 m (x, y) = safeGet x y m

-- Tests whether the given number in the matrix touches a symbol
testNum m sym is = not $ null $ intersect sym $ mapMaybe (safeGet1 m) (allNeighbors is)

-- Sums all the numbers touching symbols
solve1 m = sum $ map (indicesNum m) $ filter (testNum m symb) nums
  where nums = getNums m
        symb = symbols m

-- Get the indices of all the stars in a matrix
getStars :: Matrix Char -> [(Int, Int)]
getStars = catMaybes . toList . mapPos (\(x, y) v -> if (=='*') v then Just (x, y) else Nothing)

-- If the gear touches exactly two numbers, multiply them.
processGear :: Matrix Char -> [[(Int, Int)]] -> (Int, Int) -> Maybe Int
processGear m nums gear = case touching of 
                          x:y:[] -> Just (x * y)
                          _      -> Nothing
  where touching = map (indicesNum m) (touchingNums gear nums)

-- Get all the numbers touching the given index
touchingNums :: (Int, Int) -> [[(Int, Int)]] -> [[(Int, Int)]]
touchingNums gear = filter (not . null . intersect (getNeighbors gear))

-- Get the sum of all the valid gear ratios
solve2 m = sum $ mapMaybe (processGear m nums) stars
  where nums = getNums m
        stars = getStars m

readFileLines :: FilePath -> IO [String]
readFileLines = fmap lines . readFile

main :: IO ()
main = do
  contents <- readFileLines "../inputs/day3.txt"
  let mat = fromLists contents
  print $ solve1 mat
  print $ solve2 mat

ex = "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598.."

exmat = fromLists $ lines ex
