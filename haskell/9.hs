import Data.List

getDifferences :: [Int] -> Maybe ([Int], [Int])
getDifferences l
  | all (==0) differences = Nothing
  | otherwise             = Just (differences, differences)
    where differences = zipWith (-) (tail l) l
  
allDifferences :: [Int] -> [[Int]]
allDifferences = unfoldr getDifferences 

predict :: [Int] -> Int
predict l = sum $ map last differences
  where differences = l : allDifferences l

solver1 = sum . map predict

extrapolate :: [Int] -> Int
extrapolate l = foldr1 (-) $ map head differences
  where differences = l : allDifferences l

solver2 = sum . map extrapolate

main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day9.txt"
  let lists = map (map read . words) contents :: [[Int]]
  print $ solver1 lists
  print $ solver2 lists

tests = [[0, 3, 6, 9, 12, 15],
         [1, 3, 6, 10, 15, 21],
         [10, 13, 16, 21, 30, 45]] :: [[Int]]
