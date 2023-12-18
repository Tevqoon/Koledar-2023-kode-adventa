import Data.NumInstances.Tuple ()
import Data.Maybe (isJust, fromJust)
import Data.List
import Algorithm.Search
import qualified Debug.Trace as T
import Data.Char (intToDigit)
import Data.Array
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Coord = (Int, Int)
data Direction = N | S | E | W deriving (Show, Eq, Enum, Ord, Ix)
type Color = String
type Instruction = (Direction, Int, Color)
type Board = [(Coord, Color)]

printBoard :: Board -> IO ()
printBoard b = mapM_ putStrLn [[if (x, y) `elem` points then '#' else '.' | y <- [y1..y2]] | x <- [x1..x2]]
  where
    points = map fst b
    (xs, ys) = unzip points
    (x1, x2) = (minimum xs, maximum xs)
    (y1, y2) = (minimum ys, maximum ys)

toTuple :: Direction -> Coord
toTuple N = (-1, 0)
toTuple S = (1, 0)
toTuple E = (0, 1)
toTuple W = (0, -1)

charToDir :: String -> Direction
charToDir "U" = N
charToDir "D" = S
charToDir "L" = W
charToDir "R" = E

lineToInstr :: String -> Instruction
lineToInstr l = (charToDir dirchar, read amount, color)
  where [dirchar, amount, color] = words l

parse = map lineToInstr

step :: (Board, Coord) -> Instruction -> (Board, Coord)
step (b, pos) (dir, steps, c) = (b ++ points, fst $ last points)
  where points = [(pos + (toTuple dir) * (dpos, dpos), c) | dpos <- [1..steps]] :: [(Coord, Color)]

trench :: [Instruction] -> Board
trench = fst . foldl' step ([], (0, 0))

rayTest :: [Coord] -> Coord -> Bool
rayTest cs (cx, cy) = all (\x -> x `mod` 2 == 0) $ map length [updn, ltrt]
  where
    ltrt = filter (\(x, y) -> x == cx) cs
    updn = filter (\(x, y) -> y == cy) cs

countPoints :: Board -> Int
countPoints b = length $ filter (rayTest points) $ (range (lowBound, higBound)) \\ points
  where points = map fst b
        (xs, ys) = unzip points
        lowBound = (minimum xs, minimum ys)
        higBound = (maximum xs, maximum ys)


solver1 instrs = length t + countPoints t
  where t = trench instrs
        

main :: IO ()
main = do
  contents <- (parse . lines) <$> readFile "../inputs/day18.txt"
  print $ countPoints $ trench contents
  
test = parse ["R 6 (#70c710)",
        "D 5 (#0dc571)",
        "L 2 (#5713f0)",
        "D 2 (#d2c081)",
        "R 2 (#59c680)",
        "D 2 (#411b91)",
        "L 5 (#8ceee2)",
        "U 2 (#caa173)",
        "L 1 (#1b58a2)",
        "U 2 (#caa171)",
        "R 2 (#7807d2)",
        "U 3 (#a77fa3)",
        "L 2 (#015232)",
        "U 2 (#7a21e3)"]

tinytest = parse ["R 2 1", "D 2 2", "L 2 3", "U 2 4"]
