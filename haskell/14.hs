import Data.List
import Debug.Trace qualified as T
import Data.Maybe
import Data.Matrix as A
import Data.NumInstances.Tuple
import Data.Bifunctor (bimap)
import Data.Set qualified as S
import Data.Map.Strict qualified as M

type Coord = (Int, Int)
type Coords = S.Set Coord
type Dimensions = (Int, Int)
data Board = Board {circles :: S.Set Coord, cubes :: S.Set Coord, dimensions :: Dimensions} deriving (Eq, Ord, Show)

inBounds :: Dimensions -> Coord -> Bool
inBounds (xm, ym) (x, y) = 0 <= x && x < xm && 0 <= y && y < ym

parse :: [String] -> Board
parse b = (Board (S.fromList circles) (S.fromList squares) dims)
  where coords = [(c, (x, y)) | (l, x) <- zip b [0..], (c, y) <- zip l [0..], c /= '.']
        (circles, squares) = bimap (map snd) (map snd) $ partition (\(c, _) -> c == 'O') coords
        dims = bimap (succ . maximum) (succ . maximum) $ unzip $ map snd coords

getLoad :: Board -> Int
getLoad (Board cis cus (xm, ym)) = S.foldr (\(x, y) s -> s + xm - x) 0 cis

roll :: Coord -> Board -> Board
roll direction (Board cis cus ds) = Board (rollThese cus cis) cus ds
  where
    rollThese :: Coords -> Coords -> Coords 
    rollThese has adds 
      | S.null adds = has S.\\ cus
      | otherwise   = rollThese (S.union has nextStill) (S.union movers checkAgain)
        where
         nextUp = S.map (+direction) adds
         
         (nextInb, nextOob) = S.partition (inBounds ds) nextUp

         (stillInb, moveCandidates) = S.partition (`S.member` has) nextInb 

         nextStill = S.map (\x -> x - direction) $ S.union nextOob $ stillInb -- these are still for sure

         (nonmovers, movers) = S.partition (`S.member` adds) moveCandidates

         checkAgain = S.map (\x -> x - direction) $ nonmovers

rollNorth = roll (-1, 0)
rollSouth = roll (1, 0)
rollEast = roll (0, 1)
rollWest = roll (0, -1)
    
solver1 :: Board -> Int
solver1 = getLoad . rollNorth 

oneCycle :: Board -> Board
oneCycle = rollEast . rollSouth . rollWest . rollNorth

findFirstDuplicate :: (Enum b, Ord k, Num b) => [k] -> (k, b, b)
findFirstDuplicate lst = aux M.empty lst 0
  where
    aux seen (x:xs) rep = case foundmaybe of
      Nothing  -> aux (M.insert x rep seen) xs (succ rep)
      Just org -> (x, org, rep)
      where foundmaybe = M.lookup x seen

solver2 n l = getLoad b
  where allCycles = iterate oneCycle l
        (b, org, rep) = findFirstDuplicate $ allCycles
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

boardToMat :: Board -> Matrix Char
boardToMat b@(Board cis cus (dx, dy)) = A.mapPos (\pos _ ->
                                                    if (pos-(1,1)) `S.member` cis
                                                    then 'O' else
                                                      if (pos-(1,1)) `S.member` cus
                                                      then '#' else
                                                        '.')
                                        (A.zero dx dy)

printList :: [String] -> IO ()
printList l = (mapM_ putStrLn l) >> (putStrLn "")

printBoard = printList . toLists . boardToMat
 
