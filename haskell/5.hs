import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read

type Range = (Int, Int)
type Entry = (Range, Int)

-- Transforms a line of dest range, source range, length into an entry
lineToRanges :: String -> Entry
lineToRanges numLine = case numList of
                       [d, s, r] -> ((s, s + r - 1), d)
  where numList = map (read::String->Int) $ words numLine

-- Returns a where between each element we put the result of the given
-- function on the two elements. The first and last arguments are supplied
-- to the function for before and after the list.
intersperseBy :: (t -> t -> Maybe t) -> t -> t -> [t] -> [t]
intersperseBy f first last []     = mapMaybe (uncurry f) [(first, last)]
intersperseBy f first last (x:xs) = case f first x of
  Just fx -> fx : x : intersperseBy f x last xs
  Nothing ->      x : intersperseBy f x last xs

-- If the right interval is strictly larger than the left, insert a new one
addInterval ((s1x, s1y), _) ((s2x, s2y), _)
  | s2x == succ s1y = Nothing
  | otherwise       = Just ((s1y + 1, s2x - 1), s1y + 1)

-- Add all the missing intervals
addMissing :: [Entry] -> [Entry]
addMissing = intersperseBy addInterval ((-1, -1), -1) ((maxBound::Int, -1), -1)

-- Processes each almanac map
processMap :: String -> [Entry]
processMap = addMissing . sort . map lineToRanges . tail . lines

-- Takes the file and splits it into a list of seeds and list of maps
processLines :: String -> ([Int], [[Entry]])
processLines input = (map read $ words seednums, map processMap maps)
  where seeds:maps = splitOn "\n\n" input
        [_, seednums] = splitOn ": " seeds

-- Returns the intersection of two ranges or nothing if it doesn't exist
intersectRanges :: Range -> Range -> Maybe Range
intersectRanges (x1, y1) (x2, y2)
  | x <= y    = Just (x, y)
  | otherwise = Nothing
  where (x, y) = (max x1 x2, min y1 y2)

-- Gives the range overlapping the entry, if it exists.
checkAssoc :: Range -> Entry -> Maybe Range
checkAssoc (x, y) ((s1, s2), d) = do
  (x, y) <- intersectRanges (x, y) (s1, s2)
  return (d + x - s1, d + y - s1)

-- Takes a range and gives the ranges corresponding to it
destinations :: Range -> [Entry] -> [Range]
destinations = mapMaybe . checkAssoc

-- Lifts the destinations function to work on a list of ranges
rangesDest :: [Range] -> [Entry] -> [Range]
rangesDest = (. (flip destinations)) . (>>=)
-- rangeDest range entries = range >>= flip destination entries

-- Walks each range through the maps monadically
totalDestination :: [Range] -> [[Entry]] -> [Range]
totalDestination = foldl rangesDest 

-- Groups up the seeds and returns the corresponding intervals
seedsToRange :: [Int] -> [Range]
seedsToRange = map (\[x, y] -> (x, x + y - 1)) . chunksOf 2

-- Takes the smallest numer (which is always a left interval limit) after transforming
solver :: [Range] -> [[Entry]] -> Int
solver seedRanges = minimum . map fst . totalDestination seedRanges

main :: IO ()
main = do
  -- contents <- readFile "../inputs/ex5.txt"
  contents <- readFile "../inputs/day5.txt"
  let (seeds, maps) = processLines contents
  let seedRanges = seedsToRange seeds
  print $ solver (map (\x -> (x, x)) seeds) maps
  print $ solver seedRanges maps
