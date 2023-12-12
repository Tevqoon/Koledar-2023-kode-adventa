import Debug.Trace
import Data.List
import Data.Bifunctor
import Data.List.Split
import Data.Function.Memoize
  
parse :: String -> (String, [Int]) 
parse line = (record, (map read $ splitOn "," nums))
  where [record, nums] = words line

isN :: String -> Int -> Maybe String
isN str n
  | test      = Just retbat
  | otherwise = Nothing
  where (front, back) = splitAt n str
        l = length front == n
        test = l && all (/='.') front && (null back || (head back) /= '#')
        retbat = if null back then [] else tail back

finder :: (String, [Int]) -> Int
finder = uncurry $ memoFix2 recurse
  where
    recurse :: (String -> [Int] -> Int) -> String -> [Int] -> Int
    recurse f records [] = if (all (/='#') records) then 1 else 0
    recurse f [] (n:ns) = 0
    
    recurse f ('.':rs) ns = f (dropWhile (=='.') rs) ns
    
    recurse f records ns | (sum ns) > (length $ filter (/='.') records) = 0
    recurse f ('#':rs) (n:ns) = case cut of
      Nothing  -> 0
      Just rs' -> f rs' ns
      where cut = isN rs (pred n)
 
    recurse f ('?':rs) (n:ns) = case cut of
      Nothing  -> f rs (n:ns)
      Just rs' -> f rs (n:ns) + f rs' ns
      where cut = isN rs (pred n)

solver1 = sum . map finder

unfoldRecord = bimap (intercalate "?" . replicate 5) (concat . replicate 5)

solver2 = sum . map (finder . unfoldRecord)

main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day12.txt"
  let parsed = map parse contents
  print $ solver1 parsed
  print $ solver2 parsed

  
test = map parse ["???.### 1,1,3",
                  ".??..??...?##. 1,1,3",
                  "?#?#?#?#?#?#?#? 1,3,1,6",
                  "????.#...#... 4,1,1",
                  "????.######..#####. 1,6,5",
                  "?###???????? 3,2,1"]
