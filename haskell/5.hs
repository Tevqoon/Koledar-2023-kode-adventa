import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read

type AlmanacMap = String
type Range = (Int, Int)
type Entry = (Range, Range)

lineToRanges :: String -> Entry
lineToRanges numLine = case numList of
                       [d, s, r] -> ((s, s + r - 1), (d, d + r - 1))
  where numList = map (read::String->Int) $ splitOn " " numLine

-- Returns a where between each element we put the result of the given
-- function on the two elements. The first and last arguments are supplied
-- to the function for before and after the list.
intersperseBy :: (t -> t -> Maybe t) -> t -> t -> [t] -> [t]
intersperseBy f first last []     = mapMaybe (\(x, y) -> f x y) [(first, last)]
intersperseBy f first last (x:xs) = case f first x of
  Just fx -> fx : x : intersperseBy f x last xs
  Nothing ->      x : intersperseBy f x last xs

-- If the right interval is strictly larger than the left, make a new one
addInterval ((s1x, s1y), d1) ((s2x, s2y), d2)
  | s2x == succ s1y = Nothing
  | otherwise       = Just ((s1y + 1, s2x - 1), (s1y + 1, s2x - 1))

-- Add all the missing intervals
addMissing :: [Entry] -> [Entry]
addMissing = intersperseBy addInterval ((-1, -1), (-1, -1)) ((maxBound::Int, -1), (-1, -1))

-- Processes each almanac map
processMap :: AlmanacMap -> [Entry]
processMap almamap = addMissing $ sort entries 
  where name:ranges = filter (not . null) $ lines almamap
        entries = map lineToRanges ranges

-- Takes the file and splits it into a list of seeds and list of maps
processLines :: String -> ([Int], [[Entry]])
processLines input = (map read $ splitOn " " seednums,
                      map (sort . processMap) maps)
  where seeds:maps = splitOn "\n\n" input
        [_, seednums] = splitOn ": "  seeds

-- Returns the intersection of two ranges or nothing if it doesn't exist
intersectRanges :: Range -> Range -> Maybe Range
intersectRanges (x1, y1) (x2, y2)
  | x <= y    = Just (x, y)
  | otherwise = Nothing
  where (x, y) = (max x1 x2, min y1 y2)

-- Assumes the range has already been intersected, returns the new values.
assocRange :: Maybe Range -> Entry -> Maybe Range
assocRange Nothing _ = Nothing
assocRange (Just (x, y)) ((s1, s2), (d1, d2)) = Just (d1 + x - s1, d2 + y - s2)

-- Gives the range overlapping the entry, if it exists.
checkAssoc :: Range -> Entry -> Maybe Range
checkAssoc range (sources, destinations) =
  assocRange (intersectRanges range sources) (sources, destinations)

-- Takes a range and gives the ranges corresponding to it
destinations :: Range -> [Entry] -> [Range]
destinations inrange entries =
  mapMaybe (checkAssoc inrange) entries

-- Lifts the destinations function to work on a list of ranges
rangesDest :: [Range] -> [Entry] -> [Range]
rangesDest ranges entries = ranges >>= (\x -> (destinations x entries))

-- Walks each range through the maps monadically
totalDestination :: [Range] -> [[Entry]] -> [Range]
totalDestination ranges transforms =
  foldl rangesDest ranges transforms

-- Groups up the seeds and returns the corresponding intervals
seedsToRange :: [Int] -> [Range]
seedsToRange = map (\[x, y] -> (x, x + y - 1)) . chunksOf 2

solver seedRanges maps = minimum $ filter (0/=) $ map fst $ totalDestination seedRanges maps

main :: IO ()
main = do
  -- contents <- readFile "../inputs/ex5.txt"
  contents <- readFile "../inputs/day5.txt"
  let (seeds, maps) = processLines contents
  let seedRanges = seedsToRange seeds
  print $ solver (map (\x -> (x, x)) seeds) maps
  print $ solver seedRanges maps
