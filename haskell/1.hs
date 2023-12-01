import Data.Char
import qualified Data.Text as T

-- Only keep digits
lineNums :: String -> String
lineNums = filter isDigit

-- Fixed point
fix :: Eq a => (a -> a) -> a -> a
fix = until =<< ((==) =<<)

-- Add back the last letter in order to facilitate examples like threeight.
-- All nonnumerics get filtered
alphas =   (map T.pack ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"])
numerics = (map T.pack ["1e", "2o", "3e", "4r", "5e", "6x", "7n", "8t", "9e"])

-- Make a list of replacers and fold fix over them.
replaceAlphas :: String -> String
replaceAlphas input = T.unpack $ foldr fix (T.pack input) (zipWith T.replace alphas numerics)

-- Sum the first and last digits in each string
solver :: [String] -> Int
solver = sum . map (\(x:xs) -> read [x, last xs])

main :: IO ()
main = do
  contents <- readFile "../inputs/day1.txt"
  let nums = lines contents
  print $ solver $ map lineNums nums
  print $ solver $ map (lineNums . replaceAlphas) nums
  

