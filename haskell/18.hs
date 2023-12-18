import Data.NumInstances.Tuple ()
import Data.Char (isHexDigit)
import Data.Ix (Ix, range)
import Numeric (readHex)

type Coord = (Int, Int)
data Direction = N | S | E | W deriving (Show, Eq, Enum, Ord, Ix)
type Color = String
type Instruction = (Direction, Int)
type Board = [Coord]

printBoard :: Board -> IO ()
printBoard points = mapM_ putStrLn [[if (x, y) `elem` points then '#' else '.'
                                    | y <- [y1..y2]] | x <- [x1..x2]]
  where
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
lineToInstr l = (charToDir dirchar, read amount)
  where [dirchar, amount, color] = words l

numToDir "0" = E
numToDir "1" = S
numToDir "2" = W
numToDir "3" = N

lineToInstr' :: String -> Instruction
lineToInstr' l = (direction, hexamount)
  where [dirchar, amount, color] = words l
        instr' = filter isHexDigit color
        direction = numToDir $ pure (last instr')
        hexamount = fst $ head $ readHex $ init instr'

parse :: [String] -> [Instruction]
parse = map lineToInstr

parse' :: [String] -> [Instruction]
parse' = map lineToInstr'

-- Adds the next boundary point from the given instruction
step :: (Board, Coord) -> Instruction -> (Board, Coord)
step (b, pos) (dir, steps) = (b ++ [point], point)
  where point = pos + (toTuple dir) * (steps, steps)

-- Gives the boundary points of the dug trench
trench :: [Instruction] -> Board
trench = fst . foldl step ([], (0, 0))

-- Calculates the area of a polygon given the coordinates of its vertices.
shoelace :: [Coord] -> Int
shoelace points = abs $ (`div` 2) $ sum $ zipWith (*) ysums xdifs
  where (x:xs, y:ys) = unzip points
        ysums = zipWith (+) (y:ys) (ys ++ [y])
        xdifs = zipWith (-) (x:xs) (xs ++ [x])        

-- Integer points inside the area with the given number of boundary points
pickInside :: Int -> Int -> Int
pickInside area boundary = area - (boundary `div` 2) + 1

tupleToNum :: Coord -> Int
tupleToNum (a, b) = abs a + abs b

-- Gives the number of boundary points from a list of vertices
boundaryLength :: [Coord] -> Int
boundaryLength (p:ps) = sum $ map tupleToNum $ zipWith (-) (p:ps) (ps ++ [p])

solver instructions = boundary + pickInside area boundary
  where points = trench instructions
        boundary = boundaryLength points 
        area = shoelace points

main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day18.txt"
  print $ solver $ parse  contents
  print $ solver $ parse' contents
  
test = ["R 6 (#70c710)",
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

tinytest = parse ["R 2 1", "D 2 2", "R 1 3", "D 2 4", "L 3 5", "U 4 6"]
