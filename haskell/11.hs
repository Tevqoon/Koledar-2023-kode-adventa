import Data.List
import Data.Matrix (mapPos, toList, fromLists)
import Data.Maybe

type Coord = (Int, Int)
type Board = [[Char]]

getCoords :: Board -> [Coord]
getCoords = catMaybes . toList . mapPos (\pos c -> if c == '#' then Just pos else Nothing) . fromLists

manhattan :: Coord -> Coord -> Int
manhattan (x1, y1) (x2, y2) = abs(x1 - x2) + abs(y1 - y2)

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

getRows :: Board -> [Int]
getRows = catMaybes . zipWith (\n l -> if all (=='.') l then Just n else Nothing) [0..]

getCols :: Board -> [Int]
getCols = getRows . transpose

expandRow :: Int -> Int -> [Coord] -> [Coord]
expandRow by r = map (\(x, y) -> if x > r then (x + by, y) else (x,y))

expandCol :: Int -> Int -> [Coord] -> [Coord]
expandCol by c = map (\(x, y) -> if y > c then (x, y + by) else (x,y))

expandWith :: (Int -> Int -> [Coord] -> [Coord]) -> Int -> [Int] -> [Coord] -> [Coord]
expandWith f by [] coords     = coords
expandWith f by (x:xs) coords = expandWith f by (map (+by) xs) (f by x coords)

expand :: Int -> Board -> [Coord]
expand by board = exCols . exRows $ coords
  where coords = getCoords board
        exRows = expandWith expandRow by (getRows board)
        exCols = expandWith expandCol by (getCols board)

solver :: Int -> Board -> Int
solver by = sum . map (uncurry manhattan) . pairs . expand by

main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day11.txt"
  print $ solver 1 contents
  print $ solver (1000000 - 1) contents

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

