import Data.Set qualified as S
import Debug.Trace
import Data.Matrix qualified as A
import Data.NumInstances.Tuple ()
import Data.List
import Data.Maybe

type Coord = (Int, Int)
data Direction = N | S | E | W deriving (Show, Eq, Ord)
type Particle = (Coord, Direction)

toTuple :: Direction -> Coord
toTuple N = (-1, 0)
toTuple S = (1, 0)
toTuple E = (0, 1)
toTuple W = (0, -1)

type Dimensions = (Int, Int)
type Board = A.Matrix Char

-- A safe get which takes in a pair instead of two values
safeGet1 :: A.Matrix a -> Coord -> Maybe a
safeGet1 m (x, y) = A.safeGet x y m

reflect :: Direction -> Char -> [Direction]
reflect N '/' = [E]
reflect N '\\' = [W]
reflect N '-' = [W, E]
reflect N _ = [N]

reflect S '/' = [W]
reflect S '\\' = [E]
reflect S '-' = [W, E]
reflect S _ = [S]

reflect E '/' = [N]
reflect E '\\' = [S]
reflect E '|' = [N, S]
reflect E _ = [E]

reflect W '/' = [S]
reflect W '\\' = [N]
reflect W '|' = [N, S]
reflect W _ = [W]

next :: Board -> Particle -> S.Set Particle
next b p@(pos, d) = case mirror of 
  Nothing -> S.empty -- we would go out of the matrix, stop iterating
  Just m  -> S.fromList [(pos', d')
                        | d' <- reflect d m , let pos' = pos + toTuple d', isJust $ safeGet1 b pos']
  where mirror = safeGet1 b pos

allSpaces :: Particle -> Board -> S.Set Coord
allSpaces start b = aux (S.singleton start) (S.singleton start)
  where aux ps cs
          | S.null ps = S.map fst cs
          | otherwise = aux (step S.\\ cs) (S.union cs step)
          where step = S.unions $ S.map (next b) ps

solver1 :: Board -> Particle -> Int
solver1 b start = length $ allSpaces start b

solver2 :: A.Matrix Char -> Int
solver2 b = maximum $ map (solver1 b) edge
  where (mx, my) = (A.nrows b, A.ncols b)
        edge = [((x, 1), E) | x <- [1..mx]]
               ++ [((x, my), W) | x <- [1..mx]]
               ++ [((1, y), S) | y <- [1..my]]
               ++ [((mx, y), N) | y <- [1..my]]

main :: IO ()
main = do
  contents <- (A.fromLists . lines) <$> readFile "../inputs/day16.txt"
  print $ solver1 contents ((1, 1), E)
  print $ solver2 contents

printList :: [String] -> IO ()
printList l = (mapM_ putStrLn l) >> (putStrLn "")
  
test = A.fromLists [".|...\\....",
                    "|.-.\\.....",
                    ".....|-...",
                    "........|.",
                    "..........",
                    ".........\\",
                    "..../.\\\\..",
                    ".-.-/..|..",
                    ".|....-|.\\",
                    "..//.|...."]
