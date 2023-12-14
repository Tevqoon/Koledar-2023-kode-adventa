import Data.List
import Debug.Trace
import Data.Maybe
import Data.Matrix
import Data.Array
import Data.Map.Strict qualified as M

loadLine d l = d * (length $ filter (=='O') $ l)

getLoad :: [String] -> Int
getLoad xs = sum $ zipWith loadLine [1..] $ reverse xs

getLoadM :: Matrix Char -> Int
getLoadM m = sum $ toList $ mapPos (\(x, y) v -> if v == 'O' then l - x + 1 else 0) m
  where l = length $ toLists m

-- A safe get which takes in a pair instead of two values
safeGet1 :: Matrix a -> (Int, Int) -> Maybe a
safeGet1 m (x, y) = safeGet x y m

-- Fixed point starting with some value
fix :: Eq a => (a -> a) -> a -> a
fix = until =<< ((==) =<<)

-- The direction is encoded as a pair
rollOneM :: (Int, Int) -> Matrix Char -> Matrix Char
rollOneM (x, y) board = mapPos new_state board
  where
    new_state (x', y') '.' | (board `safeGet1` (x' - x, y' - y)) == Just 'O' = 'O'
    new_state (x', y') 'O' | (board `safeGet1` (x' + x, y' + y)) == Just '.' = '.'
    new_state (x', y') symb = symb

roll dir board = fix (rollOneM dir) board

rollNorth = roll (-1, 0)
rollSouth = roll (1, 0)
rollEast = roll (0, 1)
rollWest = roll (0, -1)
    
solver1 = getLoadM . rollNorth

oneCycle = rollEast . rollSouth . rollWest . rollNorth
  
printList :: [String] -> IO ()
printList l = (mapM_ putStrLn l) >> (putStrLn "")

findFirstDuplicate :: Eq a => [a] -> (Int, Int)
findFirstDuplicate lst = (org, rep)
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
        

solver2 n l = getLoadM $ allCycles !! first_n
  where allCycles = iterate oneCycle l
        (org, rep) = findFirstDuplicate $ allCycles
        first_n = org + mod (n - org) (rep - org)
        
main :: IO ()
main = do
  contents <- (fromLists . lines) <$> readFile "../inputs/day14.txt"
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
