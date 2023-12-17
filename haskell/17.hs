{-# LANGUAGE TemplateHaskell #-}
import Data.Function.Memoize
import Data.NumInstances.Tuple ()
import Data.Maybe (isJust)
import Data.List
import qualified Debug.Trace as T
import qualified Data.Matrix as A

type Coord = (Int, Int)
data Direction = N | S | E | W deriving (Show, Eq, Enum, Ord)
deriveMemoizable ''Direction

-- A safe get which takes in a pair instead of two values
safeGet1 :: A.Matrix a -> Coord -> Maybe a
safeGet1 m (x, y) = A.safeGet x y m

parse :: [String] -> A.Matrix Int
parse = A.fromLists . (map . map) (read . pure)

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

heatLoss :: A.Matrix Int -> Int
heatLoss board = minimum [memoized (1, 1) dir 3 | dir <- [S, E]]
  where
    bounds = (A.nrows board, A.ncols board)
    memoized = memoFix3 aux

    decrOrReset current next n
      | current == next = pred n
      | otherwise       = 3

    aux :: (Coord -> Direction -> Int -> Int) -> Coord -> Direction -> Int -> Int
    aux f pos dir n
      | pos == bounds = board A.! pos
      | otherwise = T.trace (show ()) $ board A.! pos + minimum losses
      where removeCurrentP = if n == 0 then [dir] else []
            nextDirs = ([N, S, E, W] \\ [revDir dir]) \\ removeCurrentP
            losses = [f nextPos ddir (decrOrReset dir ddir n)
                     | ddir <- nextDirs
                     , let nextPos = (pos + toTuple ddir)
                     , isJust $ safeGet1 board nextPos]

main :: IO ()
main = do
  contents <- (parse . lines) <$> readFile "../inuts/day17.txt"
  print 0

tinyTest = parse ["12",
                  "13"]

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
