import Data.List
import Data.Map.Strict (Map)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap, second, first)
import Data.Char (isDigit)
import Data.Maybe 
import qualified Data.Map.Strict as M

-- We can either accept, reject, or pass to another workflow
data Name = R | Name String | A deriving (Show, Eq, Ord)

-- A comparator either filters some values or accepts all of them.
data Comparator = Comparator {attr :: Char, op :: Char, treshold :: Int} | Default deriving (Show, Eq, Ord)

-- Each workflow is comprised of checkers, which have a comparator and a name.
data Checker = Checker {comp :: Comparator, next :: Name} deriving (Show, Eq, Ord)

-- A workflow is a name and a list of checkers. The last one is always Default, but this is not checked.
type Workflow = (Name, [Checker])

-- Since we are using workflows out of order, it is convenient to store them in a map.
type Workflows = Map Name [Checker]

-- Each part has four relevant fields.
data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show, Eq, Ord)

-- A hacky way to get record type fields via the name of the field
class Getters a b where
  fromLetter :: Char -> (a -> b)

-- Parts are an instance of this.
instance Getters Part Int where
  fromLetter :: Char -> (Part -> Int)
  fromLetter 'x' = x
  fromLetter 'm' = m
  fromLetter 'a' = a
  fromLetter 's' = s

-- Parses a given name into the Name type
parseName :: String -> Name
parseName "A"  = A
parseName "R"  = R
parseName name = Name name

parseComparator :: String -> Comparator
parseComparator comp = Comparator attr op num
    where attr : op : numstr = comp
          num = read numstr :: Int

parseChecker :: String -> Checker
parseChecker check = case body of
  [name]       -> Checker Default (parseName name)
  [comp, name] -> Checker (parseComparator comp) (parseName name)
  where body = splitOn ":" check

parseWorkflow :: String -> Workflow
parseWorkflow workflow = (Name name, map parseChecker checkers)
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

opCharToOp :: Char -> (Int -> Int -> Bool)
opCharToOp '<' = (<)
opCharToOp '>' = (>)

applyComparator :: Part -> Comparator -> Bool
applyComparator _ Default = True
applyComparator part (Comparator attr op treshold) = (opCharToOp op) (fromLetter attr $ part) treshold

nextIfChecker :: Part -> Checker -> Maybe Name
nextIfChecker part (Checker comp next) = if applyComparator part comp then Just next else Nothing

nextUp :: Part -> [Checker] -> Name
nextUp part = head . mapMaybe (nextIfChecker part)

checkPart :: Workflows -> Part -> Bool
checkPart ws part = go (Name "in")
  where
    go :: Name -> Bool
    go workflow = case result of
      A -> True
      R -> False
      Name nextUp -> go (Name nextUp)
      where result = nextUp part $ ws M.! workflow

-- Filters the parts for those that fit and sums their values
solver1 workflows parts = sum . map (\(Part x m a s) -> x + m + a + s) . filter (checkPart workflows) $ parts

type Range = (Int, Int)
data PartRange = PartRange {xs :: Range, ms :: Range, as :: Range, ss :: Range} deriving (Show, Eq, Ord)

instance Getters PartRange Range where
  fromLetter :: Char -> (PartRange -> Range)
  fromLetter 'x' = xs
  fromLetter 'm' = ms
  fromLetter 'a' = as
  fromLetter 's' = ss

-- The number of elements in a closed interval
rangeLen :: Range -> Int
rangeLen (a, b) = b - a + 1

-- The individual number of possibilities in a given range is the product of the possibilities of each of its ranges
partRangePossibilities :: PartRange -> Int
partRangePossibilities (PartRange xsr msr asr ssr) = product $ map rangeLen [xsr, msr, asr, ssr] 

-- Warning: Stupid implementation ahead
-- Splits the range into those elements that fit the operation and those that don't. 
splitRange :: Range -> Int -> Char -> (Maybe Range, Maybe Range)
splitRange (a, b) treshold operation = case partition (\x -> (opCharToOp operation) x treshold) [a..b] of
  ([], ys) -> (Nothing, Just (head ys, last ys))
  (xs, []) -> (Just (head xs, last xs), Nothing)
  (xs, ys) -> (Just (head xs, last xs), Just (head ys, last ys))

-- Warning: lens required ahead
-- Simply replaces one field with another
replaceField :: PartRange -> Char -> Range -> PartRange
replaceField (PartRange xsr msr asr ssr) 'x' nrange = PartRange nrange msr asr ssr
replaceField (PartRange xsr msr asr ssr) 'm' nrange = PartRange xsr nrange asr ssr
replaceField (PartRange xsr msr asr ssr) 'a' nrange = PartRange xsr msr nrange ssr
replaceField (PartRange xsr msr asr ssr) 's' nrange = PartRange xsr msr asr nrange

-- Ugly updater which takes into account the possibility of an empty range
updatePrange :: PartRange -> Char -> (Maybe Range, Maybe Range) -> (Maybe PartRange, Maybe PartRange)
updatePrange part field (Nothing, Nothing) = (Nothing, Nothing)
updatePrange part field (Just nleft, Nothing) = (Just $ replaceField part field nleft, Nothing)
updatePrange part field (Nothing, Just nright) = (Nothing, Just $ replaceField part field nright)
updatePrange part field (Just nleft, Just nright) = (Just $ replaceField part field nleft, Just $ replaceField part field nright)

-- To split a part we update by the relevant letter. Everything gets passed to the helper functions
splitPart :: Maybe PartRange -> Comparator -> (Maybe PartRange, Maybe PartRange)
splitPart Nothing _     = (Nothing, Nothing)
splitPart (Just part) (Comparator attr op treshold) = updatePrange part attr newRanges
  where attrRange = (fromLetter attr) part
        newRanges = splitRange attrRange treshold op

-- Applying a checker means applying the comparator, routing those that fit, and passing the rest
routePart :: Maybe PartRange -> Checker -> (Maybe PartRange, Maybe (Name, PartRange))
routePart Nothing _ = (Nothing, Nothing)
routePart (Just part) (Checker Default next) = (Nothing, Just (next, part))
routePart part (Checker comp next) = (newRangeR, newRangeL >>= (\r -> Just (next, r)))
  where (newRangeL, newRangeR) = splitPart part comp

-- How a part range goes through a given workflow
-- Fold each checker successively, accumulating the rest of the elements
-- by the end of the workflow, we have consumed all of the initial range.
stepWorkflow :: PartRange -> [Checker] -> [(Name, PartRange)]
stepWorkflow part checkers = catMaybes route
  where (_, route) = mapAccumL routePart (Just part) checkers

-- A wrapper for stepWorkflow that takes the name of a workflow instead of its list of checkers
stepWorkflows :: Workflows -> (Name, PartRange) -> [(Name, PartRange)]
stepWorkflows ws (name, partRange) = stepWorkflow partRange checkers
  where checkers = ws M.! name

-- Starts off with a range of all the possible values at the initial workflow, then on each step
-- monadically accumulates the next workflow step. Accepted elements at each step get rolled into
-- the sum of possibilities, and the rejected elements are discarded.
solver2 :: Workflows -> Int
solver2 ws = go [(Name "in", PartRange (1,4000) (1,4000) (1,4000) (1,4000))] 0
  where
    go :: [(Name, PartRange)] -> Int -> Int
    go [] n = n
    go toRoute n = go rest2 (n + additionalPossibilities)
      where upNext = toRoute >>= stepWorkflows ws
            (rejected, rest1) = partition ((==R) . fst) upNext
            (accepted, rest2) = partition ((==A) . fst) rest1
            additionalPossibilities = sum $ map partRangePossibilities $ map snd $ accepted

main :: IO ()
main = do
  --(workflows, parts) <- parse <$> readFile "../inputs/test19.txt"
  (workflows, parts) <- parse <$> readFile "../inputs/day19.txt"
  print $ solver1 workflows parts
  print $ solver2 workflows

testW = "px{a<2006:qkq,m>2090:A,rfg}"
(testN, testCheckers) = parseWorkflow testW
testComp = Comparator {attr = 'a', op = '<', treshold = 2006}
testPrange = PartRange (1,4000) (1,4000) (1,4000) (1,4000)
