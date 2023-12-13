import Data.Matrix
import Data.List
import Data.List.Split
import Data.Maybe

parse :: String -> [[String]]
parse = map lines . splitOn "\n\n"

check :: [String] -> [String] -> Maybe Int
check half1 half2
  | reflects  = Just (length half1)
  | otherwise = Nothing
  where reflects = all id $ zipWith (==) (reverse half1) half2

findHorizontal :: [String] -> [Int]
findHorizontal board = mapMaybe (\i -> uncurry check $ splitAt i board) [1..length board - 1]

findVertical :: [String] -> [Int]
findVertical = findHorizontal . Data.List.transpose

sumMirs [] ver = (head ver)
sumMirs hor [] = 100 * (head hor)

solver1 boards = sum $ zipWith sumMirs horzs verts
  where horzs = map findHorizontal boards
        verts = map findVertical   boards

other '.' = '#'
other '#' = '.'

-- Just check all of the changed boards lol
allSmudges board = map (smudge board) coords
  where coords = [(x, y) | x <- [1..length board], y <- [1.. length (head board)]]
        smudge board spos = toLists $ mapPos (\pos v -> if pos == spos then other v else v) m 
          where m = fromLists board

-- Simply get all the changed boards and remove the unchanged lines. What remains is the new line.
processOne board = sumMirs totalHor totalVer
  where smudges = allSmudges board
        regularHor = findHorizontal board
        regularVer = findVertical   board
        smudgedHor = nub $ smudges >>= findHorizontal
        smudgedVer = nub $ smudges >>= findVertical
        totalHor = smudgedHor \\ regularHor
        totalVer = smudgedVer \\ regularVer

solver2 = sum . map processOne

main :: IO ()
main = do
  contents <- parse <$> readFile "../inputs/day13.txt"
  print $ solver1 contents
  print $ solver2 contents
  
test = [["#.##..##.",
         "..#.##.#.",
         "##......#",
         "##......#",
         "..#.##.#.",
          "..##..##.",
          "#.#.##.#."],
        ["#...##..#",
         "#....#..#",
         "..##..###",
         "#####.##.",
         "#####.##.",
         "..##..###",
         "#....#..#"]]
