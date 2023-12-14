import Data.List
import Data.Map.Strict qualified as M

getLoad :: [String] -> Int
getLoad xs = sum $ zipWith loadLine [1..] $ reverse xs
  where loadLine d l = d * (length $ filter (=='O') $ l)

-- Saves how many free dots we have available
-- Fills the space with maximal Os and dumps the dots when at # or end
roll1 :: String -> String
roll1 = aux 0
  where
    aux :: Int -> String -> String
    aux dots ('.':xs) = aux (succ dots) xs
    aux dots ('O':xs) = 'O' : aux dots xs
    aux dots ('#':xs) = replicate dots '.' ++ '#' : aux 0 xs
    aux dots _        = replicate dots '.'

west, east, north, south :: [String] -> [String]
west = map roll1
east = map (reverse . roll1 . reverse)
north = transpose . west . transpose
south = transpose . east . transpose

solver1 :: [String] -> Int
solver1 = getLoad . north

findDuplicate :: (Enum b, Ord k, Num b) => [k] -> (b, b)
findDuplicate lst = aux M.empty lst 0
  where
    aux seen (x:xs) rep = case foundmaybe of
      Nothing  -> aux (M.insert x rep seen) xs (succ rep)
      Just org -> (org, rep)
      where foundmaybe = M.lookup x seen

oneCycle :: [String] -> [String]
oneCycle = east . south . west . north

solver2 :: Int -> [String] -> Int
solver2 n b = getLoad $ boards !! cycled
  where boards = iterate oneCycle b
        (i, j) = findDuplicate boards
        cycled = i + (n - i) `mod` (j - i)
        
main :: IO ()
main = do
  contents <- (lines) <$> readFile "../inputs/day14.txt"
  print $ solver1 contents
  print $ solver2 1000000000 contents
