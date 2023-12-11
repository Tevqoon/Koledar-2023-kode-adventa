import Data.List
import Data.Matrix 
import Data.Maybe

type Coord = (Int, Int)
type Board = [[Char]]

copyIfEmpty :: String -> [String]
copyIfEmpty line
  | all (=='.') line = [line, line]
  | otherwise        = [line]

expandRows :: Board -> Board
expandRows = concatMap copyIfEmpty

expandCols :: Board -> Board
expandCols = Data.List.transpose . expandRows . Data.List.transpose

expandBoard :: Board -> Board
expandBoard = expandCols . expandRows

processBoard :: Board -> Matrix Char
processBoard = fromLists . expandBoard

getCoords :: Matrix Char -> [(Int, Int)]
getCoords = catMaybes . toList . mapPos (\pos c -> if c == '#' then Just pos else Nothing)

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs(x1 - x2) + abs(y1 - y2)

pairs :: [Coord] -> [(Coord, Coord)]
pairs l = [(l !! i, l !! j) | i <- [0..length l - 1], j <- [0..i - 1]]

solver1 = sum .map (uncurry manhattan) . pairs . getCoords . processBoard

main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day11.txt"
  print $ solver1 contents

test = ["...#......",
        ".......#..",
        "#.........",
        "..........",
        "......#...",
        ".#........",
        ".........#",
        "..........",
        ".......#..",
        "#...#....."]

