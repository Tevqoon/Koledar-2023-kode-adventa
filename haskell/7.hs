import Data.List
import Data.List.Split
import Data.Maybe
import Text.Read
import Data.Function

data Hand = Hand {hand :: [Int], bid :: Int} deriving (Eq, Show, Read)

instance Ord Hand where
  h1 <= h2
    | equal     = (hand h1) <= (hand h2)
    | otherwise = (scoreHand h1) <= (scoreHand h2)
    where equal = (scoreHand h1) == (scoreHand h2)

scoreHand :: Hand -> Int
scoreHand Hand {hand = hand}
  | groupNumber == 1                        = 7
  | groupNumber == 2 && (head lengths) == 4 = 6
  | groupNumber == 2 && (head lengths) == 3 = 5
  | groupNumber == 3 && (head lengths) == 3 = 4
  | groupNumber == 3 && (head lengths) == 2 = 3
  | groupNumber == 4                        = 2
  | otherwise                               = 1
  where grouped = sortBy (flip compare `on` length) $ group $ sort hand
        groupNumber = length grouped
        lengths = map length grouped

enumerateCard :: Char -> Int
enumerateCard card
  | card == 'A' = 14
  | card == 'K' = 13
  | card == 'Q' = 12
  | card == 'J' = 11
  | card == 'T' = 10
  | otherwise   = read [card]

parseHand :: String -> [Int]
parseHand = map enumerateCard

parseLine :: String -> Hand
parseLine line = Hand {hand = parseHand hand, bid = read bid}
  where [hand, bid] = words line

winnings :: Int -> Hand -> Int
winnings rank h = rank * (bid h)

solver1 = sum . zipWith winnings [1..] . sort

-- Bruh moment ahead
data JHand = JHand {jhand :: [Int], jbid :: Int} deriving (Eq, Show, Read)

instance Ord JHand where
  h1 <= h2
    | equal     = (jhand h1) <= (jhand h2)
    | otherwise = (scoreJHand h1) <= (scoreJHand h2)
    where equal = (scoreJHand h1) == (scoreJHand h2)

scoreJHand :: JHand -> Int
scoreJHand JHand {jhand = [1,1,1,1,1]} = 7
scoreJHand JHand {jhand = hand}
  | groupNumber == 1                        = 7
  | groupNumber == 2 && (head lengths) == 4 = 6
  | groupNumber == 2 && (head lengths) == 3 = 5
  | groupNumber == 3 && (head lengths) == 3 = 4
  | groupNumber == 3 && (head lengths) == 2 = 3
  | groupNumber == 4                        = 2
  | otherwise                               = 1
  where (jokers, rest) = partition (==1) hand
        strongest:grest = sortBy (flip compare `on` length) $ group $ sort rest
        grouped = (jokers ++ strongest) : grest
        groupNumber = length grouped
        lengths = map length grouped

enumerateJCard :: Char -> Int
enumerateJCard card
  | card == 'A' = 14
  | card == 'K' = 13
  | card == 'Q' = 12
  | card == 'J' = 1
  | card == 'T' = 10
  | otherwise   = read [card]

parseJHand :: String -> [Int]
parseJHand = map enumerateJCard

parseJLine :: String -> JHand
parseJLine line = JHand {jhand = parseJHand hand, jbid = read bid}
  where [hand, bid] = words line

jwinnings :: Int -> JHand -> Int
jwinnings rank h = rank * (jbid h)

solver2 = sum . zipWith jwinnings [1..] . sort

main :: IO ()
main = do
  hands <- lines <$> readFile "../inputs/day7.txt"
  -- hands <- lines <$> readFile "../inputs/ex7.txt"
  let parsed = map parseLine hands
  let jparsed = map parseJLine hands
  print $ solver1 parsed
  print $ solver2 jparsed
