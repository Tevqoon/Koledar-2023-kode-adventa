import Data.List
import Debug.Trace
import Data.Maybe
-- import Data.Matrix
import Data.Bifunctor (bimap)
import Data.Set qualified as S
import Data.Map.Strict qualified as M

-- type Board = Matrix Char
-- type Board = M.Map (Int, Int) Char
type Coord = (Int, Int)
type Dimensions = (Int, Int)
data Board = Board {circles :: S.Set Coord, cubes :: S.Set Coord, dimensions :: Dimensions} deriving (Eq, Ord, Show)

inBounds :: Dimensions -> Coord -> Bool
inBounds (xm, ym) (x, y) = 0 <= x && x < xm && 0 <= y && y < ym

isFree :: Coord -> Board -> Bool
isFree pos (Board cis cus ds)
  | not $ inBounds ds pos = False
  | otherwise             = S.notMember pos cis && S.notMember pos cus

parse :: [String] -> Board
parse b = (Board (S.fromList circles) (S.fromList squares) dims)
  where coords = [(c, (x, y)) | (l, x) <- zip b [0..], (c, y) <- zip l [0..], c /= '.']
        (circles, squares) = bimap (map snd) (map snd) $ partition (\(c, _) -> c == 'O') coords
        dims = bimap (succ . maximum) (succ . maximum) $ unzip $ map snd coords       

getLoad :: Board -> Int
getLoad (Board cis cus (xm, ym)) = S.foldr (\(x, y) s -> s + xm - x) 0 cis

-- Fixed point starting with some value
fix :: Eq a => (a -> a) -> a -> a
fix = until =<< ((==) =<<)

-- The direction is encoded as a pair
rollOne :: Coord -> Board -> Board
rollOne (x', y') b@(Board cis cus ds) = Board (S.map new_state cis) cus ds
  where
    new_state (x, y) | isFree (x' + x, y' + y) b = (x' + x, y' + y) 
    new_state symb = symb

-- roll :: Coord -> Board -> Board
-- roll dir board = fix (rollOne dir) board

rollNorth = roll (-1, 0)
rollSouth = roll (1, 0)
rollEast = roll (0, 1)
rollWest = roll (0, -1)
    
solver1 :: Board -> Int
solver1 = getLoad . rollNorth 

oneCycle :: Board -> Board
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
