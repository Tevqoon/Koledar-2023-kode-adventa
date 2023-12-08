import Data.List
import Data.List.Split
import Data.Char
import qualified Data.Map.Strict as M

type Location = String
type Command = Char
type Direction = (String, (String, String))
type Directions = M.Map Location (Location, Location)

-- Checks the third character of location
locationIs :: Char -> Location -> Bool
locationIs c = (==c) . (!!2)

-- Converts a L or R command to the relevant projection
stepToCommand :: Command -> ((a, a) -> a)
stepToCommand 'L' = fst
stepToCommand 'R' = snd

listToTuple :: [Location] -> Direction
listToTuple [a, b, c] = (a, (b, c))

-- Gives a location from the supplied line
processLine :: String -> Direction
processLine = listToTuple . chunksOf 3 . filter isAlpha

-- Cuts up the file into a cyclic list of commands, a Map of directions, and a list of A-locations
processFile :: String -> ([Command], Directions, [Location])
processFile file = (cycle steps,
                    M.fromList pairList,
                    map fst $ filter (locationIs 'A' . fst) pairList)
  where [steps, graph] = splitOn "\n\n" file
        pairList = map processLine $ lines graph

-- Does one step; maps location to the next location
step :: Directions -> Command -> Location -> Location
step graph command = (stepToCommand command) . (graph M.!) 

-- Counts the number of steps needed to go from the starting location to reach the end condition
stepper :: Directions -> (String -> Bool) -> [Command] -> Location -> Int
stepper graph condition commands location = aux 0 commands location
  where aux stepNum (command:commands) location
          | condition location = stepNum
          | otherwise = aux (succ stepNum) commands (step graph command location)

-- Number of steps from AAA to ZZZ
solver1 :: Directions -> [Command] -> Int
solver1 graph commands = stepper graph (=="ZZZ") commands "AAA"

-- Number of steps from each XXA to XXZ; keeping in mind permutation order is the lcm of its cycles
solver2 :: Directions -> [Command] -> [Location] -> Int
solver2 graph commands = foldr lcm 1 . map (stepper graph (locationIs 'Z') commands)
    
main :: IO ()
main = do
  contents <- readFile "../inputs/day8.txt"
  -- contents <- readFile "../inputs/ex8.txt"
  let (moves, graph, starting) = processFile contents
  print $ solver1 graph moves
  print $ solver2 graph moves starting
