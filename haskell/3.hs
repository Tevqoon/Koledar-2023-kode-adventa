import Data.Char
import Data.List
import Data.Matrix
import Data.Maybe
import Data.Containers.ListUtils (nubOrd)

type Indices = (Int, Int)
type IndexNum = [Indices]
type Schematic = Matrix Char
type Symbols = String

-- Returns the 9 coordinates around the given one
getNeighbors :: Indices -> [Indices]
getNeighbors (x, y) = [(x + i, y + j) | i <- [-1..1], j <- [-1..1], (i, j) /= (0, 0)]

-- Gives all the coordinates around the supplied ones
allNeighbors :: [Indices] -> [Indices]
allNeighbors = nubOrd . (getNeighbors =<<)

-- From a list of Maybe values, accumulate contiguous justs into lists and drop nothings
extractJust :: [Maybe a] -> [[a]]
extractJust [] = []
extractJust (Nothing:xs) = extractJust (dropWhile isNothing xs)
extractJust xs = (catMaybes justs) : extractJust rest
  where
    (justs, rest) = span isJust xs

-- If the predicate holds on the second argument, return the first argument.
predMaybe :: (b -> Bool) -> a -> b -> Maybe a
predMaybe p x v
  | p v       = Just x
  | otherwise = Nothing

-- Returns the number represented by the indices in the supplied list in matrix
indicesNum :: Schematic -> IndexNum -> Int
indicesNum m = read . map (m!)

-- Scans the matrix for contiguous numbers and returns their index list
getNums :: Schematic -> [IndexNum]
getNums m = filter (not . null) $ extractJust $ toList num_mat
  where num_mat =  mapPos (predMaybe isDigit) m

-- A safe get which takes in a pair instead of two values
safeGet1 :: Matrix a -> Indices -> Maybe a
safeGet1 m (x, y) = safeGet x y m

-- Tests whether the given number in the matrix touches a symbol
testNum :: Schematic -> Symbols -> IndexNum -> Bool
testNum m symb is = not $ null $ intersect symb $ mapMaybe (safeGet1 m) (allNeighbors is)

-- Extracts all nonnumeric non '.' symbols out of a given char matrix
symbols :: Schematic -> Symbols
symbols = nub . filter (not . isDigit) . filter ('.'/=) . toList

-- Sums all the numbers touching symbols
solve1 :: Schematic -> Int
solve1 m = sum . map (indicesNum m) . filter (testNum m symb) $ nums
  where nums = getNums m
        symb = symbols m

-- Get the indices of all the stars in a matrix
getStars :: Schematic -> [Indices]
getStars = catMaybes . toList . mapPos (predMaybe (=='*'))

-- If the gear touches exactly two numbers, multiply them.
processGear :: Schematic -> [IndexNum] -> Indices -> Maybe Int
processGear m nums gear = case touching of 
                          [x, y] -> Just (x * y)
                          _      -> Nothing
  where touching = map (indicesNum m) (touchingNums gear nums)

-- Get all the numbers touching the given index
touchingNums :: Indices -> [IndexNum] -> [IndexNum]
touchingNums gear = filter (not . null . intersect (getNeighbors gear))

-- Get the sum of all the valid gear ratios
solve2 :: Schematic -> Int
solve2 m = sum . mapMaybe (processGear m nums) $ stars
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
