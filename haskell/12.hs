import Debug.Trace
import Data.List
import Data.Bifunctor
import Data.List.Split
import Data.Function.Memoize
  
parse :: String -> (String, [Int]) 
parse = bimap id (map read . splitOn ",") . tuple . words
  where tuple = \[x, y] -> (x, y)

-- If the first n are # or ? and the one after is ? or ., return the rest
isN :: String -> Int -> Maybe String
isN str n
  | test      = Just retbat
  | otherwise = Nothing
  where (front, back) = splitAt n str
        l = length front == n
        test = l && all (/='.') front && (null back || (head back) /= '#')
        retbat = if null back then [] else tail back

-- A memoized recursive searcher of the possible ways
finder :: (String, [Int]) -> Int
finder = uncurry $ memoFix2 recurse
  where
    recurse :: (String -> [Int] -> Int) -> String -> [Int] -> Int

    -- Out of numbers is valid if only . and ? remain
    recurse f records [] = if (all (/='#') records) then 1 else 0

    -- No more stream and some numbers is a contradiction
    recurse f [] (n:ns) = 0

    -- We can always drop running streams of dots
    recurse f ('.':rs) ns = f (dropWhile (=='.') rs) ns

    -- Telescopic optimization; always drop leading dots
    recurse f records ns | (sum ns) > (length $ filter (/='.') records) = 0

    -- We cannot drop a #, so the only recursive case comes in the case of isN.
    recurse f ('#':rs) (n:ns) = case cut of
      Nothing  -> 0
      Just rs' -> f rs' ns
      where cut = isN rs (pred n)

    -- In the case of a ?, it is either a # or a .
    recurse f ('?':rs) (n:ns) = case cut of
      Nothing  -> f rs (n:ns)
      Just rs' -> f rs (n:ns) + f rs' ns
      where cut = isN rs (pred n)

solver1 :: [(String, [Int])] -> Int
solver1 = sum . map finder

-- Replicate both components, then join the first one with ?, and simply concatenate the second
unfoldRecord :: (String, [Int]) -> (String, [Int])
unfoldRecord = bimap (intercalate "?" . replicate 5) (concat . replicate 5)

solver2 :: [(String, [Int])] -> Int
solver2 =  sum . map (finder . unfoldRecord)

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
