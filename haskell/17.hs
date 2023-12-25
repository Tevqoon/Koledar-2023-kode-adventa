import Data.NumInstances.Tuple ()
import Data.Maybe (isJust, fromJust)
import Data.List
import Algorithm.Search
import qualified Debug.Trace as T
import Data.Char (intToDigit)
import Data.Array

type Coord = (Int, Int)
data Direction = N | S | E | W deriving (Show, Eq, Enum, Ord, Ix)

type Board = Array Coord Int

printBoard :: Board -> String
printBoard b = intercalate "\n" $ [[intToDigit $ b ! (x, y) | y <- [y1..y2]] | x <- [x1..x2]]
  where
    ((x1, y1), (x2, y2)) = bounds b

parse :: [String] -> Board
parse ls = listArray ((1, 1), (mx, my)) $ concat $ (map . map) (read . pure) ls
  where (mx, my) = (length ls, length $ head ls)

toTuple :: Direction -> Coord
toTuple N = (-1, 0)
toTuple S = (1, 0)
toTuple E = (0, 1)
  toTuple W = (0, -1)

revDir :: Direction -> Direction
revDir N = S
revDir S = N
revDir E = W
revDir W = E

turn :: Direction -> [Direction]
turn N = [W, E]
turn S = [W, E]
turn E = [N, S]
turn W = [N, S]

solver1 board = minimum $ map (fst <$>) [searchDown, searchRight]
  where
    searchDown = searcher ((1, 1), S, 3)
    searchRight = searcher ((1, 1), E, 3)
    
    bbounds@(lbound, upbound) = bounds board
    searcher = dijkstra neighbors
               (\_ (pos, _, _) -> board ! pos)
               (\(pos, _, _) -> pos == upbound)

    decrOrReset current next n
      | current == next = pred n
      | otherwise       = 3

    -- T.trace (show ((pos, dir, n), nextPoss))
    neighbors (pos, dir, n) =  nextPoss
      where keepGoing = if n /= 1 then [dir] else []
            nextDirs = keepGoing ++ (turn dir)
            nextPoss = [(nextPos, ddir, decrOrReset dir ddir n)
                       | ddir <- nextDirs
                       , let nextPos = (pos + toTuple ddir)
                       , inRange bbounds nextPos]

solver2 board = fst <$> search
  where
    search = searcher ((1, 1), N, 0)
    
    bbounds@(lbound, upbound) = bounds board2
    searcher = dijkstra neighbors
               (\_ (pos, _, _) -> board ! pos)
               (\(pos, _, n) -> pos == upbound && n <= 7)

    decrOrReset current next n
      | current == next = pred n
      | otherwise       = 10

    -- T.trace (show ((pos, dir, n), nextPoss)) $ 
    neighbors (pos, dir, n) = nextPoss
      where keepGoing = if n > 1 then [dir] else []
            turnIf = if n <= 7 then turn dir else []
            nextDirs = keepGoing ++ turnIf
            nextPoss = [(nextPos, ddir, decrOrReset dir ddir n)
                       | ddir <- nextDirs
                       , let nextPos = (pos + toTuple ddir)
                       , inRange bbounds nextPos]

    
main :: IO ()
main = do
  contents <- (parse . lines) <$> readFile "../inputs/day17.txt"
  print $ solver1 contents
  print $ solver2 contents

tinyTest = parse ["111111",
                  "111125"]

test = parse ["2413432311323",
              "3215453535623",
              "3255245654254",
              "3446585845452",
              "4546657867536",
              "1438598798454",
              "4457876987766",
              "3637877979653",
              "4654967986887",
              "4564679986453",
              "1224686865563",
              "2546548887735",
              "4322674655533"]

test2 = parse ["111111111111",
         "999999999991",
         "999999999991",
         "999999999991",
         "999999999991"]
