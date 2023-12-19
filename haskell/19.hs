import Data.List
import Data.Map.Strict (Map)
import Data.List.Split (splitOn)
import Data.Bifunctor (bimap, second)
import Data.Char (isDigit)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M

data Name = Name String | A | R deriving (Show, Eq, Ord) 
type Comparator = Part -> Bool
data Checker = Checker {fun :: Comparator, next :: Name}
type Workflow = (Name, [Checker])
type Workflows = Map Name [Checker]
data Part = Part {x :: Int, m :: Int, a :: Int, s :: Int} deriving (Show, Eq, Ord)

fromLetter :: Char -> (Part -> Int)
fromLetter 'x' = x
fromLetter 'm' = m
fromLetter 'a' = a
fromLetter 's' = s

parseName :: String -> Name
parseName "A"  = A
parseName "R"  = R
parseName name = Name name

makeComparator :: String -> Comparator
makeComparator comp = \part -> op (fromLetter attr $ part) num
    where attr : opstr : numstr = comp
          op = case opstr of
                 '<' -> (<)
                 '>' -> (>)
          num = read numstr :: Int

parseChecker :: String -> Checker
parseChecker check = case body of
  [name]       -> Checker (const True) (parseName name)
  [comp, name] -> Checker (makeComparator comp) (parseName name)
  where body = splitOn ":" check

parseWorkflow :: String -> Workflow
parseWorkflow workflow = (Name name, map parseChecker checkers)
  where
    [name, rest] = splitOn "{" workflow
    checkers = splitOn "," $ init rest

parsePart :: String -> Part
parsePart part = Part x m a s
  where [x, m, a, s] = map (read . filter isDigit) $ splitOn "," part

parse1 :: String -> (Workflows, [Part])
parse1 = bimap (M.fromList . map parseWorkflow . lines) (map parsePart . lines) . tuple . splitOn "\n\n"
  where tuple = \[x, y] -> (x, y)

nextUp :: Part -> [Checker] -> Name
nextUp part = next . fromJust . find (\checker -> fun checker $ part)

checkPart :: Workflows -> Part -> Bool
checkPart ws part = go (Name "in")
  where
    go :: Name -> Bool
    go workflow = case result of
      A -> True
      R -> False
      Name nextUp -> go (Name nextUp)
      where result = nextUp part $ fromJust $ M.lookup workflow ws 
    

solver1 workflows parts = sum . map (\(Part x m a s) -> x + m + a + s) . filter (checkPart workflows) $ parts


main :: IO ()
main = do
  --(workflows, parts) <- parse1 <$> readFile "../inputs/test19.txt"
  (workflows, parts) <- parse1 <$> readFile "../inputs/day19.txt"
  print $ solver1 workflows parts

testW = "px{a<2006:qkq,m>2090:A,rfg}"
