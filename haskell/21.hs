import Data.Matrix (Matrix)
import qualified Data.Matrix as A
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Either
import Data.NumInstances.Tuple ()
import Data.Bifunctor
import Data.Map (Map)
import qualified Data.Map as M
import qualified Debug.Trace as T
import Data.Tuple (swap)

import Control.Monad.RWS

type Coord = (Int, Int)
type Bounds = (Int, Int)

parse :: [String] -> (Bounds, Coord, Set Coord)
parse = separateStart . catMaybes . A.toList
  . A.mapPos (\pos v -> case v of
                 '#' -> Nothing
                 '.' -> Just (Right pos)
                 'S' -> Just (Left pos)
                 ) .  A.fromLists
  where
    separateStart :: [(Either Coord Coord)] -> (Bounds, Coord, Set Coord)
    separateStart coords = (b, head start, S.fromList (start ++ rest))
      where (start, rest) = partitionEithers coords
            b@(bx, by) = bimap (maximum . map fst) (maximum . map snd) (rest, rest)

neighbors :: Set Coord -> Coord -> Set Coord
neighbors stepmap pos = S.intersection possible stepmap
  where possible = S.fromList [pos + dx | dx <- [(0, 1), (0, -1), (-1, 0), (1, 0)]]

step :: Set Coord -> Set Coord -> Set Coord
step stepmap poss = S.unions $ S.map (neighbors stepmap) poss

-- Runs the walker for n steps; returns the number of reached tiles
walk :: RWS (Int, Set Coord) () (Int, Set Coord) (Int)
walk = do
  (stepnum, current) <- get
  (m, stepmap) <- ask
  if stepnum >= m then
    return (length current)
    else do
    let next = step stepmap current
    put (stepnum + 1, next)
    walk

solver1 :: Set Coord -> Int -> Coord -> [Int]
solver1 stepmap n start = [a]
  where (a, s, w) = runRWS walk (n, stepmap) (0, S.singleton start)

-- Seems like the starting tile is always in the middle and the border is always empty.
-- Should be easier to fit the things together this way.

-- Reader : A set of valid coordinates in the board
-- Writer : ()
-- State  : (stepNum, previous, current)
-- If the next state 
fillUp :: RWS (Set Coord) () (Int, Set Coord, Set Coord) ((Int, Int), (Int, Int))
fillUp = do
  (stepnum, previous, current) <- get
  stepmap <- ask
  let next = step stepmap current
  case next == previous of
    True -> return ((stepnum - 1, length previous), (stepnum, length current)) -- The stabilization begins at the previous step.
    False -> do
      put (stepnum + 1, current, next)
      fillUp
    
fillUpWrapped :: Set Coord -> Coord -> [((Int, Int), (Int, Int))]
fillUpWrapped stepmap start = [a]
  where (a, s, w) = runRWS fillUp stepmap (0, S.empty, S.singleton start)

-- Calculates the area of the Von Neumann neighborhood with radius r.
vonNeumann :: Int -> Int
vonNeumann r = r^2 + (r + 1)^2

getCells :: Set Coord -> Int -> Coord -> [Set Coord]
getCells stepmap n start = [s]
  where (a, (_, s), w) = runRWS walk (n, stepmap) (0, S.singleton start)

-- I need the following function: for a given inner radius, it calculates the number of odd and even points, assuming the start
-- is of a given type.

splitOE :: [Int] -> ([Int],[Int])
splitOE []       = ([],[])
splitOE [x]      = ([x],[])
splitOE (x:y:xs) = (x:odds, y:evens)   
    where 
        (odds, evens) = splitOE xs

-- Returns two numbers : the nubmer of those cells the same as the center + the number of different ones
-- radius_helper :: Int -> (Int, Int)
radius_helper inner_radius = bimap sum sum $ splitOE [4 * (k + 1) | k <- [0..inner_radius - 1]]

solver2 steps b =
  starting
  + same_as_start_n * same_as_start
  + diff_from_start_n * diff_from_start
  + (from_left + from_top + from_right + from_bottom)
  + boundary_section * (from_bottom_or_right + from_bottom_or_left + from_top_or_right + from_top_or_left)
  + corner_section * (top_left + top_right + bottom_left + bottom_right)
                     
  where
    (total_radius, remainder) = steps `divMod` b
    inner_radius = total_radius - 1
    innerArea = (vonNeumann inner_radius) - 1

    (same_as_start_n, diff_from_start_n) = radius_helper inner_radius

    boundary = (vonNeumann total_radius) - innerArea - 1
    boundary_section = (boundary - 4) `div` 4

    corner_section = boundary_section + 1
    (top_left, top_right, bottom_left, bottom_right) = (959, 982, 973, 979)
                       
    (starting_odd, starting_even) = (7388, 7424)
    starting = if odd steps then starting_odd else starting_even
      
    inner@(inner_odd, inner_even) = (7424, 7388)
    (same_as_start, diff_from_start) = if odd steps then swap inner else inner
    
    (from_left, from_top, from_right, from_bottom) = (5650, 5665, 5680, 5665)
    (from_bottom_or_right, from_bottom_or_left, from_top_or_right, from_top_or_left) = (6556, 6533, 6548, 6541)

-- 607661709677824 = vonNeumann 202300 * 7424; a sort of upper bound

-- 606187694825670 -> solver2 with the total radius equal to the divMod (too low)

-- 606187687542870
-- 606187702108470 -- still too low
-- 606188475096770 -- still wrong!!

-- 606193687787048 -> solver2 with the total radius equal to the divMod + 1 (too high)


main :: IO ()
main = do
  ((b, _), start, stepmap) <- (parse . lines) <$> readFile "../inputs/day21.txt"
  -- print $ solver1 64 start stepmap
  -- print $ fillUpWrapped stepmap start
  -- Gives [((129,7388),(130,7424))] 

  -- print $ [(66, 1), (1, 66), (66, 131), (131, 66)] >>= (fillUpWrapped stepmap)
  -- Gives [((194,7388),(195,7424)),((194,7388),(195,7424)),((194,7388),(195,7424)),((194,7388),(195,7424))]
  -- This means that each board after 194 steps starting from any of the edges is filled, and from there on only oscillates.

  -- The number of steps is 26501365.
  -- divMod 26501365 131 = (202300,65).
  -- This seems to indicate that we can just exactly reach 202300 tiles away?
  -- Specifically, the number of tiles remaining for the outermost ring is 131.

  let [p_from_left, p_from_top, p_from_right, p_from_bottom] = [(66, 1), (1, 66), (66, 131), (131, 66)] >>= (getCells stepmap 131)
  let (from_bottom_or_right, from_bottom_or_left, from_top_or_right, from_top_or_left) =
        (length $ S.union p_from_bottom p_from_right,
         length $ S.union p_from_bottom p_from_left,
         length $ S.union p_from_top p_from_right,
         length $ S.union p_from_top p_from_left)

  -- print $ (from_bottom_or_right, from_bottom_or_left, from_top_or_right, from_top_or_left)
  -- gives (6556,6533,6548,6541), which is indeed larger than the cardinal directions themselves
  
  -- print $ [(66, 1), (1, 66), (66, 131), (131, 66)] >>= (solver1 stepmap 131)
  print $ solver1 stepmap 
  -- returns [5650,5665,5680,5665], which are the number of tiles reachable in 131 steps
  -- from each of the cardinal directions. This is relevant only for the outermost ring
  -- Specifically : [from left : 5650, from top: 5665, from right : 5680, from bottom: 5665]

  -- Plan: Calculate the von neumann neighborhood to get the inner area and the outermost ring
  -- The inner area is then the center + half of the rest of each of the odd and even squares
  -- The outer ring has:
    -- one of each of the cardinal directions for 131; these are only reachable from one side
    -- Every other one is reachable from 2 places: WARNING: we might need to check the unions of the valid sides instead of the max
    -- in fact, this is exactly what we must do.

  -- We also need to account for the fact that we can slighlty leave the area.
  -- For this reason, let's also check how far we can get from each corner to 65 in
  -- let corners = [(1, 1), (1,131), (131, 1), (131, 131)] >>= (solver1 stepmap 65)
  -- top left : 959, top right : 982, bottom left: 973, bottom right: 979
  -- print corners

  print 0

test = parse ["...........",
              ".....###.#.",
              ".###.##..#.",
              "..#.#...#..",
              "....#.#....",
              ".##..S####.",
              ".##..#...#.",
              ".......##..",
              ".##.#.####.",
              ".##..##.##.",
              "..........."]
