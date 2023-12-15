import Data.Char (ord, isAlpha)
import Data.List.Split (splitOn)
import Data.List
import Data.Maybe
import Data.IntMap.Strict qualified as M

type Label = String
type FocalLength = Int
type StoredLens = (Label, FocalLength)

type Lenses = M.IntMap [StoredLens]

parse :: String -> [String]
parse = splitOn "," . head . lines 

hash :: String -> Int
hash = aux 0
  where
    aux n [] = n
    aux n (c:cs) = aux (((n + (ord c)) * 17) `mod` 256) cs

solver1 :: [String] -> Int
solver1 = sum . map hash    

replace :: Int -> a -> [a] -> [a]
replace i x xs = xs1 ++ [x] ++ (tail xs2)
  where (xs1, xs2) = splitAt i xs

adder :: StoredLens -> Maybe [StoredLens] -> Maybe [StoredLens]
adder lens Nothing = Just [lens]
adder (label, focal) (Just lenses) = case findIndex (\(x,y) -> x == label) lenses of
  Nothing    -> Just $ lenses ++ [(label, focal)]
  Just index -> Just $ replace index (label, focal) lenses

remover :: Label -> Maybe [StoredLens] -> Maybe [StoredLens]
remover label Nothing = Nothing
remover label (Just lenses) = case findIndex (\(x, y) -> x == label) lenses of
  Nothing    -> Just lenses
  Just index -> if null new then Nothing else Just new
    where (xs1, xs2) = splitAt index lenses
          new = (xs1 ++ (tail xs2))

apply :: String -> Lenses -> Lenses
apply instrString
  | control == '=' = M.alter (remover label) (hash label)
  | control == '-' = M.alter (adder (label, read lens)) (hash label) 
  where (label, control : lens) = span isAlpha instrString

power :: Int -> [StoredLens] -> Int
power k = sum . zipWith (\x (_, focal) -> x * focal * (k + 1)) [1..]

mSum :: M.IntMap Int -> Int
mSum = M.foldr (+) 0

solver2 :: [String] -> Int
solver2 = mSum . M.mapWithKey power . foldl' (flip apply) M.empty 

main :: IO ()
main = do
  contents <- readFile "../inputs/day15.txt"
  let parsed = parse contents
  print $ solver1 parsed
  print $ solver2 parsed

test = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
