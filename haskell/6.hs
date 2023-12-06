import Data.List
import Data.Maybe
  
beatsRace :: Int -> Int -> Int -> Bool
beatsRace time distance strategy = (time - strategy) * strategy > distance

goodStrategies :: Int -> Int -> Int
goodStrategies time distance = lastBeat - firstBeat + 1
  where firstBeat = fromJust $ find (beatsRace time distance) [0..time]
        lastBeat  = fromJust $ find (beatsRace time distance) [time,time-1..0]

lineNums :: String -> [Int]
lineNums = map read . tail . words

lineNum :: String -> Int
lineNum = read . foldr (++) "" . tail . words

main :: IO ()
main = do
  [rawtimes, rawdistances] <- lines <$> readFile "../inputs/day6.txt"
  let (times, distances) = (lineNums rawtimes, lineNums rawdistances)
  let (time, distance) = (lineNum rawtimes, lineNum rawdistances)
  print $ product $ zipWith goodStrategies times distances
  print $ goodStrategies time distance
  

