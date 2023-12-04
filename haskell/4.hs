import Data.Char
import Data.List.Split
import Data.List
import Text.Read
import Data.Maybe

data Ticket = Ticket {winning :: [Int], have :: [Int]} deriving (Show)

-- Gives a list of the numbers space-separated in the given string
getNumbers :: String -> [Int]
getNumbers = mapMaybe (readMaybe::String -> Maybe Int) . splitOn " "

-- converts a whole ticket line into the ticket record
lineToTicket :: String -> Ticket
lineToTicket line =
  Ticket {winning = getNumbers winning_str,
          have = getNumbers have_str}
  where [_, winning_str, have_str] = splitOneOf ":|" line

-- Calculates the number of winning numbers on a ticket
matching :: Ticket -> Int
matching Ticket {winning=winning, have=have} = length $ intersect winning have

-- Scores the ticket given the first ruling
score1 :: Int -> Int
score1 n
  | n == 0    = 0
  | otherwise = 2 ^ (pred n)

-- Scores all the given winning numbers
solver1 :: [Int] -> Int
solver1 = sum . map score1

-- Applies the current ticket to the counts of the following tickets
incrNext :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
incrNext (have, wins) list =
  (map (\(x, y) -> (x + have, y)) to_incr) ++ rest
  where (to_incr, rest) = splitAt wins list

-- Walks over the tickets and incrementally applies the winnings
walker :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
walker processed [] = processed
walker processed (next:to_process) = walker (next:processed) (incrNext next to_process)

-- Counts the number of tickets after a complete run
solver2 :: [Int] -> Int
solver2 = sum . map fst . walker [] . zip (repeat 1)

readFileLines :: FilePath -> IO [String]
readFileLines = fmap lines . readFile

main :: IO ()
main = do
  contents <- readFileLines "../inputs/day4.txt"
  let tickets = map (matching . lineToTicket) contents
  print $ solver1 tickets
  print $ solver2 tickets

ex = ["Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
      "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
      "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
      "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
      "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
      "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"]

mex = map (matching . lineToTicket) ex     
