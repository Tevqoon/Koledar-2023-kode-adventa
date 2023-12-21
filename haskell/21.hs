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

-- Read : All valid points
-- Write : A map of points to their distance to the starting point ()
-- State : The set of the neighbors we're still to process
-- Return : Nothing, it is all contained in the state
bfs :: RWS (Set Coord) (Map Coord Int) (Int, Set Coord, Set Coord) ()
bfs = do
  (n, to_process, seen) <- get
  if S.null to_process then return ()
    else do
    tell (M.fromSet (const n) to_process)
    
    stepmap <- ask
    let next = (step stepmap to_process) S.\\ seen
    put (n + 1, next, S.union seen next)
    bfs    

bfsWrap stepmap start = w
  where (a, (_, s, _), w) = runRWS bfs (stepmap) (0, S.singleton start, S.empty)

solver1 stepmap start steps = length $ M.filter (<= steps) $ M.filter even $ bfsWrap stepmap start

-- "Derived" the formula from paper and previous experiments
-- The input has the ceintral vertical and horizontal lines empty, and the start point is in the exact center.
-- 
solver2 stepmap bound start steps = a + b - c + d
  where visited = bfsWrap stepmap start
        radius = steps `div` bound
        halfway = bound `div` 2
  
        evens = M.filter even visited
        odds  = M.filter odd  visited

        even_corners = M.filter (>halfway) evens
        odd_corners  = M.filter (>halfway) odds

        a = (radius + 1)^2 * (length odds)
        b = radius^2 * (length evens)
        c = (radius + 1) * (length odd_corners)
        d = radius * (length even_corners)

main :: IO ()
main = do
  ((b, _), start, stepmap) <- (parse . lines) <$> readFile "../inputs/day21.txt"
  print $ solver1 stepmap start 64
  print $ solver2 stepmap b start 26501365

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
