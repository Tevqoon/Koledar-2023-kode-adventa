import Data.List
import Debug.Trace
import Data.Maybe

loadLine d l = d * (length $ filter (=='O') $ l)

getLoad :: [String] -> Int
getLoad xs = sum $ zipWith loadLine [1..] $ reverse xs

rollOneUnit :: Char -> Char -> (Char, Char)
rollOneUnit 'O' '.' = ('.', 'O')
rollOneUnit x y = (x, y)

-- first argument rolls into the second one
rollOneLine :: String -> String -> (String, String)
rollOneLine from to = unzip $ zipWith rollOneUnit from to

-- Rolls the new string into the rest of them, recursively
rollOne :: String -> [String] -> [String]
rollOne x [] = [x]
rollOne x (y:ys) = x' : (rollOne y' ys)
  where (x', y') = rollOneLine x y

rollNorth :: [String] -> [String]
rollNorth = aux []
  where
    aux a [] = reverse a
    aux [] (x:xs) = aux [x] xs
    aux as (x:xs) = aux (rollOne x as) xs

rollSouth = reverse . rollNorth . reverse

rollWest = transpose . rollNorth . transpose

rollEast = transpose . reverse . rollNorth . reverse . transpose

solver1 = getLoad . rollNorth

oneCycle = rollEast . rollSouth . rollWest . rollNorth
  
printList :: [String] -> IO ()
printList l = (mapM_ putStrLn l) >> (putStrLn "")

findFirstDuplicate lst = (org, rep)
  where rep = fromJust $ findIndex id $ zipWith elem lst (inits lst)
        org = fromJust $ elemIndex (lst !! rep) lst

solver2 n l = getLoad $ allCycles !! first_n
  where allCycles = iterate oneCycle l
        (org, rep) = findFirstDuplicate $ allCycles
        first_n = trace (show (org, rep)) $ org + mod (n - org) (rep - org)
        
main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day14.txt"
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
