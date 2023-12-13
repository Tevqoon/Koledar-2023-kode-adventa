import Data.List
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

parse :: String -> [[String]]
parse = map lines . splitOn "\n\n"

-- Number of different characters of two strings
hamming :: String -> String -> Int
hamming s1 s2 = length $ filter id $ zipWith (/=) s1 s2

-- Checks if the current halves form a mirror with exactly tol smudges
-- Entering 1 will mean the old reflection is no longer valid by default, which exactly finds the smudge.
check :: Int -> [String] -> Int -> Bool
check tol board n = totalDist == tol
  where totalDist = sum $ zipWith hamming (reverse half1) half2
        (half1, half2) = splitAt n board

-- Finds a mirror index. If not, returns 0
findHorizontal :: Int -> [String] -> Int
findHorizontal tol board = fromMaybe 0 $ find (check tol board) [1..length board - 1]

-- Vertical splits are horizontal splits of the transpose
findVertical :: Int -> [String] -> Int
findVertical = (.transpose) . findHorizontal

-- We can sum all because non-mirrors are 0
solver tol boards = sum horzs + sum verts
  where horzs = map ((*100) . findHorizontal tol) boards
        verts = map (findVertical tol)   boards

main :: IO ()
main = do
  contents <- parse <$> readFile "../inputs/day13.txt"
  print $ solver 0 contents
  print $ solver 1 contents
  
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
