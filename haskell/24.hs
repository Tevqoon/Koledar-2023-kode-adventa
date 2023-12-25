import Data.NumInstances.Tuple ()
import Control.Monad.RWS
import Data.List
import Data.List.Split (splitOn)
import Data.Bifunctor
import Data.Maybe
import Data.Char
import Numeric.LinearAlgebra
import Control.Monad (guard)

type Coord = (Double, Double, Double)
type Pos = Coord
type Vel = Coord
type Bounds = (Double, Double)

parse :: String -> (Pos, Vel)
parse = bimap xyz xyz . pv
  where
    pv = tuple2 . splitOn "@"
    xyz = tuple3 . map read . splitOn ","
    tuple2 = \[x, y] -> (x, y)
    tuple3 = \[x, y, z] -> (x, y, z)

inBounds :: Bounds -> Coord -> Bool
inBounds (minC, maxC) (x, y, z) =
  (minC <= x && x <= maxC)
  && (minC <= y && y <= maxC)
  && (minC <= z && z <= maxC)

intersect2 :: Bounds -> (Pos, Vel) -> (Pos, Vel) -> Maybe Coord
intersect2 bounds ((x1, y1, _), (vx1, vy1, _)) ((x2, y2, _), (vx2, vy2, _)) = do
  [a, b] <- toList . flatten <$> linearSolve mat aff
  let (x, y) = (x1, y1) + (a, a) * (vx1, vy1)
  guard (a > 0 && b > 0)
  guard (inBounds bounds (x, y, y))
  return (x, y, 0)
  where
    mat = (2><2) [vx1, -vx2, vy1, -vy2]
    aff = col [x2 - x1, y2 - y1]

pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

solver1 :: Bounds -> [(Pos, Vel)] -> Int
solver1 bounds = length . mapMaybe (uncurry $ intersect2 bounds) . pairs

main :: IO ()
main = do
  contents <- (map parse . lines) <$> readFile "../inputs/day24.txt"

  print $ solver1 (200000000000000, 400000000000000) contents

test = map parse ["19, 13, 30 @ -2,  1, -2",
                  "18, 19, 22 @ -1, -1, -2",
                  "20, 25, 34 @ -2, -2, -4",
                  "12, 31, 28 @ -1, -2, -1",
                  "20, 19, 15 @  1, -5, -3"]

test1 = test !! 0
test2 = test !! 1
test3 = test !! 2
test4 = test !! 3
test5 = test !! 4
