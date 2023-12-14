import Data.List
import Debug.Trace qualified as T
import Data.Maybe
import Data.Matrix as A
import Data.NumInstances.Tuple
import Data.Bifunctor (bimap)
import Data.Set qualified as S
import Data.Map.Strict qualified as M

-- type Board = Matrix Char
-- type Board = M.Map (Int, Int) Char
type Coord = (Int, Int)
type Coords = S.Set Coord
type Dimensions = (Int, Int)
data Board = Board {circles :: S.Set Coord, cubes :: S.Set Coord, dimensions :: Dimensions} deriving (Eq, Ord, Show)

boardToMat :: Board -> Matrix Char
boardToMat b@(Board cis cus (dx, dy)) = A.mapPos (\pos _ -> if (pos-(1,1)) `S.member` cis then 'O' else
                                               if (pos-(1,1)) `S.member` cus then '#' else '.') (A.zero dx dy)

printList :: [String] -> IO ()
printList l = (mapM_ putStrLn l) >> (putStrLn "")

printBoard = printList . toLists . boardToMat

inBounds :: Dimensions -> Coord -> Bool
inBounds (xm, ym) (x, y) = 0 <= x && x < xm && 0 <= y && y < ym

-- isFree :: Dimensions -> Coords -> Coords -> Coord -> Coord -> Bool
-- isFree ds cis cus direction pos
--   | not $ inBounds ds (direction + pos) = False
--   | otherwise = S.notMember pos cis && S.notMember pos cus

parse :: [String] -> Board
parse b = (Board (S.fromList circles) (S.fromList squares) dims)
  where coords = [(c, (x, y)) | (l, x) <- zip b [0..], (c, y) <- zip l [0..], c /= '.']
        (circles, squares) = bimap (map snd) (map snd) $ partition (\(c, _) -> c == 'O') coords
        dims = bimap (succ . maximum) (succ . maximum) $ unzip $ map snd coords

getLoad :: Board -> Int
getLoad (Board cis cus (xm, ym)) = S.foldr (\(x, y) s -> s + xm - x) 0 cis

coordUpdate :: Coords -> Coords -> Coords -> Coords
coordUpdate coords toRemove toAdd = S.union toAdd $ coords S.\\ toRemove

-- Premature optimization is the root of all evil.
partition3 :: (a -> Ordering) -> S.Set a -> (S.Set a, S.Set a, S.Set a)
partition3 f s = (S.filter (\x -> f x == LT) s, S.filter (\x -> f x == EQ) s, S.filter (\x -> f x == GT) s)

roll :: Coord -> Board -> Board
roll direction@(dx, dy) b@(Board cis cus ds) = Board (rollThese cis cis) cus ds
  where
    rollThese :: Coords -> Coords -> Coords 
    rollThese has adds 
      | S.null adds = has
      | otherwise   = rollThese (coordUpdate has moving moved) (S.union moved goodChecks)
        where
          (still, checkAgain, moving) = partition3 (isFree . (+direction)) adds
          moved = S.map (+direction) moving
          goodChecks = S.filter (\x -> (x + direction) `S.member` checkAgain || (x + direction) `S.member` moving) checkAgain
          -- Those over the edge or moving into # are never moving again.
          -- Those that are moving into O need to be checked again.
          -- TODO: Make sure to account for fixed elements; something like remove those that still can't move
          -- and have been there last turn already
          -- Those with free spaces are free to move
          isFree pos
            | not $ inBounds ds pos = LT
            | S.member pos cus = LT
            | S.member pos has = EQ
            | otherwise = GT
              

rollNorth = roll (-1, 0)
rollSouth = roll (1, 0)
rollEast = roll (0, 1)
rollWest = roll (0, -1)
    
solver1 :: Board -> Int
solver1 = getLoad . rollNorth 

oneCycle :: Board -> Board
oneCycle = rollEast . rollSouth . rollWest . rollNorth
  
findFirstDuplicate :: Eq a => [a] -> (Int, Int)
findFirstDuplicate lst = T.trace (show (org, rep)) (org, rep)
  where rep = fromJust $ findIndex id $ zipWith elem lst (inits lst)
        org = fromJust $ elemIndex (lst !! rep) lst

-- In practice, finding the duplicate is not really a bottleneck as
-- opposed to the actual rotations.

-- findFirstDuplicate :: (Enum b, Ord k, Num b) => [k] -> (b, b)
-- findFirstDuplicate lst = aux M.empty lst 0
--   where
--     aux seen (x:xs) rep = case foundmaybe of
--       Nothing  -> aux (M.insert x rep seen) xs (succ rep)
--       Just org -> (org, rep)
--       where foundmaybe = M.lookup x seen
        

solver2 n l = getLoad $ allCycles !! first_n
  where allCycles = iterate oneCycle l
        (org, rep) = findFirstDuplicate $ allCycles
        first_n = org + mod (n - org) (rep - org)
        
main :: IO ()
main = do
  contents <- (parse . lines) <$> readFile "../inputs/day14.txt"
  print $ solver1 contents
  print $ solver2 1000000000 contents
  

test = ["O....#....",
        "O.OO#....#",
        ".....##...",
        "OO.#O....O",
        ".O.....O#.",
        "O.#..O.#.#",
        "..O..#O..O",
        ".......O..",
        "#....###..",
        "#OO..#...."]

l1 = "O....#...."
l2 = "O.OO#....#"

rolledTest = ["OOOO.#.O..",
              "OO..#....#",
              "OO..O##..O",
              "O..#.OO...",
              "........#.",
              "..#....#.#",
              "..O..#.O.O",
              "..O.......",
              "#....###..",
              "#....#...."]
