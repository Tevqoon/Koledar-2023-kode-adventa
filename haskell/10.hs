import Data.List
import Data.Matrix
import Data.Maybe

type Coord = (Int, Int)
type Board = Matrix Char


-- A safe get which takes in a pair instead of two values
safeGet1 :: Matrix a -> Coord -> Maybe a
safeGet1 m (x, y) = safeGet x y m

-- Checks that the element at given coordinate is in the given list of characters.
-- safeGet ensures coordinates outside boundary gracefully return False.
isGood :: Board -> (Coord, [Char]) -> Bool
isGood m (c, a) = case (safeGet1 m c) of
  Nothing -> False
  Just p  -> p `elem` a

-- From (current, previous) gives (next, current), allowing self composition for calculating the loop.
getNext :: Board -> (Coord, Coord) -> (Coord, Coord)
getNext m ((x, y), previous) = (head possible, (x, y))
  where possible = case m ! (x, y) of
          '|' -> [(x + 1, y), (x - 1, y)] \\ [previous]
          '-' -> [(x, y + 1), (x, y - 1)] \\ [previous]
          'L' -> [(x, y + 1), (x - 1, y)] \\ [previous]
          'J' -> [(x - 1, y), (x, y - 1)] \\ [previous]
          '7' -> [(x + 1, y), (x, y - 1)] \\ [previous]
          'F' -> [(x + 1, y), (x, y + 1)] \\ [previous]
          '.' -> undefined
          'S' -> map fst $ filter (isGood m) [((x, y - 1), ['L', '-', 'F']),
                                              ((x, y + 1), ['J', '-', '7']),
                                              ((x + 1, y), ['|', 'J', 'L']),
                                              ((x - 1, y), ['|', '7', 'F'])]

-- Returns loop coordinates by taking from an iterator
findLoop :: Board -> [Coord]
findLoop m = map fst $ w : takeWhile (\(x, _) -> x /= start) ws
  where w:ws = iterate (getNext m) (start, (0, 0))
        start = findStart m

-- Returns the coordinates to 'S'.
findStart :: Board -> Coord
findStart m = fromJust $ fromJust $ find isJust $ toList positions
  where
    positions = mapPos (\c v -> if v == 'S' then Just c else Nothing) m

-- The furthest point on the start of the loop is exactly |loop| / 2 points away
solver1 :: [Coord] -> Int
solver1 loop = (length loop) `div` 2

-- Calculates the area of a polygon given the coordinates of its vertices.
-- Very handy to keep in mind.
shoelace :: [Coord] -> Int
shoelace points = (`div` 2) $ abs $ sum (zipWith (*) (x:xs) (ys ++ [y])) - sum (zipWith (*) (y:ys) (xs ++ [x])) 
  where (x:xs, y:ys) = unzip points

-- Pick's formula is stated as A = i + (b / 2) - 1, where
-- i = # of inside integer points
-- b = # of boundary integer points
-- of a given polygon.
-- We can thus calculate the number of inside points from its area and boundary points.

-- Pick's formula reversed.
pickInside :: Int -> Int -> Int
pickInside area boundary = area - (boundary `div` 2) + 1

-- Uses the shoelace formula for the area and inserts it into Pick's formula to get integer points inside boundary.
solver2 :: [Coord] -> Int
solver2 points = pickInside area (length points)
  where area = shoelace points

main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day10.txt"
  let mat = fromLists contents
  let loop = findLoop mat
  print $ solver1 loop
  print $ solver2 loop
