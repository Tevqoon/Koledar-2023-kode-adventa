import Data.List.Split (splitOn, chunksOf)
import Data.Char (isAlpha)
import Data.Bifunctor (bimap)
import qualified Data.Map.Strict as M

type Location = String
type Commands = String
type Directions = M.Map Location (Location, Location)

-- Ints are a monoid under lcm
instance Semigroup Int where (<>) = lcm
instance Monoid Int where mempty = 1

isC :: Char -> Location -> Bool
isC = (.last) . (==)

-- Cuts up the file into a cyclic list of commands and a Map of directions
parse :: String -> (Commands, Directions)
parse = bimap cycle (M.fromList . map graph . lines) . tuple . splitOn "\n\n"
  where graph = (\[a,b,c]->(a,(b,c))) . chunksOf 3 . filter isAlpha
        tuple = \[x, y] -> (x, y)

-- Counts the number of steps needed to go from the starting location to reach the end condition
steps :: (Commands, Directions) -> Location -> Int
steps (commands, graph) = aux 0 commands
  where aux n (c:cs) l
          | isC 'Z' l = n
          | otherwise = aux (succ n) cs step
          where step = next $ graph M.! l
                next = if c == 'L' then fst else snd

-- Number of steps from AAA to ZZZ
solver1 :: (Commands, Directions) -> Int
solver1 = flip steps "AAA"

-- Number of steps from each XXA to XXZ; lcm works on input
solver2 :: (Commands, Directions) -> [Location] -> Int
solver2 = foldMap . steps
    
main :: IO ()
main = do
  navi@(_, graph) <- parse <$> readFile "../inputs/day8.txt"
  print $ solver1 navi
  let starting = (filter (isC 'A') $ M.keys graph)
  print $ solver2 navi starting
