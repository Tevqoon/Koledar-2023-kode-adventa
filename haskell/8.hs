import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char

type Location = String
type Command = Char
type Direction = (String, (String, String))

stepToCommand :: Command -> ((a, a) -> a)
stepToCommand 'L' = fst
stepToCommand 'R' = snd
stepToCommand c   = error $ "Wrong char: " ++ (show c)

listToTuple :: [Location] -> Direction
listToTuple [a, b, c] = (a, (b, c))

processLine :: String -> Direction
processLine = listToTuple . chunksOf 3 . filter isAlpha

processFile :: String -> ([Command], [Direction])
processFile file = (cycle steps, map processLine $ lines graph)
  where [steps, graph] = splitOn "\n\n" file

step :: [Direction] -> Command -> Location -> Location
step graph command location = (stepToCommand command) $ fromJust $ lookup location graph

stepper :: [Direction] -> (String -> Bool) -> [Command] -> Location -> Int
stepper graph condition commands location = aux 0 commands location
  where aux stepNum (command:commands) location
          | condition location = stepNum
          | otherwise = aux (succ stepNum) commands (step graph command location)

solver1 :: [Direction] -> [Command] -> Int
solver1 graph commands = stepper graph (=="ZZZ") commands "AAA"

locationIs :: Char -> Location -> Bool
locationIs c = (==c) . (!!2)

solver2 :: [Direction] -> [Command] -> [Location] -> Int
solver2 graph commands = foldr lcm 1 . map (stepper graph (locationIs 'Z') commands)
    
  
main :: IO ()
main = do
  contents <- readFile "../inputs/day8.txt"
  -- contents <- readFile "../inputs/ex8.txt"
  let (moves, graph) = processFile contents
  print $ solver1 graph moves
  let starting = map fst $ filter (locationIs 'A' . fst) graph
  print $ solver2 graph moves starting
