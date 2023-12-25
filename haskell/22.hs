{-# Language MonadComprehensions #-}
import Data.Set (Set)
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Bifunctor
import Data.Maybe
import Data.NumInstances.Tuple()

type Coord = (Int, Int, Int)
type Block = (Coord, Coord)

parseLine :: Int -> String -> Block
parseLine num line = (bimap coords coords bounds)
  where
    bounds = tuple2 . splitOn "~" $ line
    coords = tuple3 . map read . splitOn ","
    
    tuple3 = \[x,y,z] -> (z, y, x) -- z first so sorting gives the blocks bottom to top
    tuple2 = \[x,y] -> (x, y)

parse :: [String] -> [Block]
parse ls = zipWith parseLine [1..] ls

intersects :: Block -> Block -> Bool
intersects ((x1, y1, z1), (x2, y2, z2)) ((x1', y1', z1'), (x2', y2', z2')) =
    overlap x1 x2 x1' x2' && overlap y1 y2 y1' y2' && overlap z1 z2 z1' z2'
    where
        overlap a1 a2 b1 b2 = a1 <= b2 && b1 <= a2

-- stabilize :: [Block] -> [Block]
stabilize = foldl (aux False) (0, []) . sort
  where
    aux fell (n, xs) x
      | Just next <- lower x, all (not . intersects next) xs = aux True (n, xs) next
      | otherwise = (n + if fell then 1 else 0, x : xs)

lower :: Block -> Maybe Block     
lower ((z1, y1, x1), (z2, y2, x2)) = [ ((z1 - 1, y1, x1), (z2 - 1, y2, x2)) | z1 > 1 ]

getAbove :: [Block] -> Block -> [Block]
getAbove blocks block = filter (intersects above) (blocks \\ [block])
  where above = block + ((1, 0, 0), (1, 0, 0))

getBelow :: [Block] -> Block -> [Block]
getBelow blocks block = filter (intersects below) (blocks \\ [block])
  where below = block - ((1, 0, 0), (1, 0, 0))

canRemove :: [Block] -> Block -> Bool
canRemove blocks block = not $ any null belows
  where aboves = getAbove blocks block
        belows = map ((\\[block]) . getBelow blocks) aboves
 
solver1 stabilized = length $ filter (canRemove stabilized) stabilized

solver2 stabilized = sum $ map (fst . stabilize) ways
  where ways = [stabilized \\ [x] | x <- stabilized]
                     
main :: IO ()
main = do
  contents <- (parse . lines) <$> readFile "../inputs/day22.txt"
  let (_, stabilized) = stabilize contents
  print $ solver1 stabilized
  print $ solver2 stabilized

test = parse ["1,0,1~1,2,1",
                  "0,0,2~2,0,2",
                  "0,2,3~2,2,3",
                  "0,0,4~0,2,4",
                  "2,0,5~2,2,5",
                  "0,1,6~2,1,6",
                  "1,1,8~1,1,9"]
test2 = parse ["1,0,2~1,0,3", "3,0,2~3,0,3", "1,0,4~3,0,4"]
