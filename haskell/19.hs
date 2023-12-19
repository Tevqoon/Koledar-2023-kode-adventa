{-# LANGUAGE TemplateHaskell #-}

import Control.Lens (Lens', makeLenses, (^.), set)
import Data.List (partition, mapAccumL)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap, second)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe, catMaybes)
import Data.Map.Strict (Map)
import Data.Tuple (swap)
import qualified Data.Map.Strict as M

class FromLetter f g where
  fromLetter :: Char -> Lens' f g

-- We can either accept, reject, or pass to another workflow
data Address = A | R | Address String deriving (Eq, Ord, Show)

-- A comparator either filters some values or accepts all of them.
data Checker = Checker {attr :: Char, opr :: Char, threshold :: Int, next :: Address} | Default Address

-- A workflow is a name and a list of checkers. The last one is always Default, but this is not checked.
type Workflow = (Address, [Checker])

-- Since we are using workflows out of order, it is convenient to store them in a map.
type Workflows = Map Address [Checker]

-- Each part has four relevant fields.
data Part = Part {_x :: Int, _m :: Int, _a :: Int, _s :: Int} deriving (Show, Eq, Ord)

makeLenses ''Part

instance FromLetter Part Int where
  fromLetter 'x' = x
  fromLetter 'm' = m
  fromLetter 'a' = a
  fromLetter 's' = s

-- Parses a given name into the Address type
parseName :: String -> Address
parseName "A"  = A
parseName "R"  = R
parseName name = Address name

parseChecker :: String -> Checker
parseChecker check = case splitOn ":" check of
  [name]       -> Default (parseName name)
  [comp, name] -> Checker attr opr num (parseName name)
    where attr : opr : numstr = comp
          num = read numstr :: Int

opr' :: Char -> (Int -> Int -> Bool)
opr' '<' = (<)
opr' '>' = (>)
         
parseWorkflow :: String -> Workflow
parseWorkflow workflow = (Address name, map parseChecker checkers)
  where
    [name, rest] = splitOn "{" workflow
    checkers = splitOn "," $ init rest

-- We assume the attributes are always listed in order
parsePart :: String -> Part
parsePart part = Part x m a s
  where [x, m, a, s] = map (read . filter isDigit) $ splitOn "," part

parse :: String -> (Workflows, [Part])
parse = bimap (M.fromList . map parseWorkflow . lines) (map parsePart . lines) . tuple . splitOn "\n\n"
  where tuple = \[x, y] -> (x, y)

applyComparator :: Part -> Checker -> Maybe Address
applyComparator part (Default next) = Just next
applyComparator part (Checker attr opr threshold next) =
  if (opr' opr) (part^.(fromLetter attr)) threshold then Just next else Nothing

nextUp :: Part -> [Checker] -> Address
nextUp part = head . mapMaybe (applyComparator part)

checkPart :: Workflows -> Part -> Bool
checkPart ws part = go (Address "in")
  where
    go :: Address -> Bool
    go workflow = case result of
      A -> True
      R -> False
      Address nextUp -> go (Address nextUp)
      where result = nextUp part $ ws M.! workflow

-- Filters the parts for those that fit and sums their values
solver1 :: Workflows -> [Part] -> Int
solver1 workflows = sum . map (\(Part x m a s) -> x + m + a + s) . filter (checkPart workflows)

type Range = (Int, Int)
data PartRange = PartRange {_xs :: Range, _ms :: Range, _as :: Range, _ss :: Range}
  deriving (Show, Eq, Ord)

makeLenses ''PartRange

instance FromLetter PartRange Range where
  fromLetter :: Char -> Lens' PartRange Range
  fromLetter 'x' = xs
  fromLetter 'm' = ms
  fromLetter 'a' = as
  fromLetter 's' = ss

partRangePossibilities :: PartRange -> Int
partRangePossibilities (PartRange xsr msr asr ssr) =
  product $ map rangeLen [xsr, msr, asr, ssr]
  where rangeLen (a, b) = b - a + 1

splitRange :: Range -> Char -> Int -> (Maybe Range, Maybe Range)
splitRange (a, b) opr threshold
  | a > b = (Nothing, Nothing) -- Invalid range
  | otherwise = case opr of
      '>' -> (makeRange (max (threshold + 1) a) b, makeRange a (min threshold b))
      '<' -> (makeRange a (min (threshold - 1) b), makeRange (max threshold a) b)
      _   -> (Nothing, Nothing) -- Invalid operator
  where
    makeRange x y
      | x > y     = Nothing
      | otherwise = Just (x, y)

updatePrange :: PartRange -> Char -> (Maybe Range, Maybe Range)
  -> (Maybe PartRange, Maybe PartRange)
updatePrange part field = bimap replacer replacer
  where replacer = (>>= (\r -> Just $ set (fromLetter field) r part))

routePart :: Maybe PartRange -> Checker
  -> (Maybe PartRange, Maybe (Address, PartRange))
routePart Nothing      _                      = (Nothing, Nothing)
routePart (Just part) (Default next)          = (Nothing, Just (next, part))
routePart (Just part) (Checker attr opr threshold next) = second (fmap (next,)) updated
  where updated = updatePrange part attr $ newRanges
        attrRange = part^.(fromLetter attr)
        newRanges = swap $ splitRange attrRange opr threshold

stepWorkflows :: Workflows -> (Address, PartRange) -> [(Address, PartRange)]
stepWorkflows ws (name, part) = catMaybes route
  where (_, route) = mapAccumL routePart (Just part) checkers
        checkers = ws M.! name
        
solver2 :: Workflows -> Int
solver2 ws = go [(Address "in", PartRange (1,4000) (1,4000) (1,4000) (1,4000))] 0
  where
    go :: [(Address, PartRange)] -> Int -> Int
    go [] n = n
    go toRoute n = go rest (n + additionalPossibilities)
      where upNext = toRoute >>= stepWorkflows ws
            (accepted, rest) = partition ((==A) . fst) $ filter ((/=)R . fst) upNext
            additionalPossibilities = sum $ map partRangePossibilities $ map snd $ accepted

main :: IO ()
main = do
  -- (workflows, parts) <- parse <$> readFile "../inputs/test19.txt"
  (workflows, parts) <- parse <$> readFile "../inputs/day19.txt"
  print $ solver1 workflows parts
  print $ solver2 workflows

testW = "px{a<2006:qkq,m>2090:A,rfg}"
(testN, testCheckers) = parseWorkflow testW
-- `testComp = Comparator {attr = 'a', fun = (<2006)}
testPrange = PartRange (1,4000) (1,4000) (1,4000) (1,4000)
