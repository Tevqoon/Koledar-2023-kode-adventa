{-# LANGUAGE TemplateHaskell #-}

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, letter, spaces)
import Text.Parsec.Combinator (sepBy)

import Data.NumInstances.Tuple ()

import Control.Lens
import Data.Maybe
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Sequence (Seq ((:|>), (:<|)), (<|), (|>), (><))
import qualified Data.Sequence as Seq

import Data.List

import qualified Debug.Trace as T

import Control.Monad.RWS

data PulseType = High | Low deriving (Eq, Ord, Show, Enum)

other :: PulseType -> PulseType
other High = Low
other Low = High

type Pulse = (Address, Address, PulseType)
type Address = String
data Module = Broadcaster {_name :: Address, _targets :: [Address]}
            | FlipFlop    {_name :: Address, _previous :: PulseType, _targets :: [Address]}
            | Conjunction {_name :: Address, _inputs :: Map Address PulseType, _targets :: [Address]} deriving (Eq, Ord, Show)
makeLenses ''Module

type Modules = Map Address Module

-- Parsing 
address :: Parser Address
address = many1 letter

arrow :: Parser String
arrow = spaces >> string "->" >> spaces >> return "" <?> "arrow parser"

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (spaces >> char ',' >> spaces)

broadcaster :: Parser Module
broadcaster = do
   name <- address
   _ <- arrow
   targets <- commaSep address
   return (Broadcaster name targets)
   <?> "broadcaster"

flipflop :: Parser Module
flipflop = do
  _ <- char '%'
  name <- address
  _ <- arrow
  targets <- commaSep address
  return (FlipFlop name Low targets)
  <?> "flip-flop"

conjunction :: Parser Module
conjunction = do
  _ <- char '&'
  name <- address
  _ <- arrow
  targets <- commaSep address
  return (Conjunction name M.empty targets)
  <?> "conjunction"

parseModule :: Parser Module
parseModule = try broadcaster <|> try flipflop <|> try conjunction

parseInput :: String -> Modules
parseInput = makeModules . addInputs . mapMaybe (either (const Nothing) Just) . map (parse parseModule "") . lines
  where makeModules = M.fromList . map (\x -> (x^.name, x))

isConjunction :: Module -> Bool
isConjunction (Conjunction _ _ _) = True
isConjunction _ = False

-- Adds the inputs to the list of 
addInputs :: [Module] -> [Module]
addInputs modules = map (\x -> if isConjunction x then updateConjunction x else x) modules
  where initInputs (Conjunction address _ _)
          = M.fromList . map (\m -> (m^.name, Low)) . filter (elem address . (^.targets)) $ modules
        updateConjunction conj = set inputs (initInputs conj) conj

-- Sending
type MyState = (Seq Pulse, Modules)
type MyRWS = RWS [String] ((Sum Int, Sum Int), [String]) MyState

-- Applying a pulse to a module
pulseOne :: Module -> Pulse -> (Module, Seq Pulse)
pulseOne b@(Broadcaster name targets) (_, _, pulse) = (b, Seq.fromList . map (,name,pulse) $ targets)

pulseOne f@(FlipFlop name prev targets) (_, _, High) = (f, Seq.Empty)
pulseOne f@(FlipFlop name Low  targets) (_, _, Low) =
  (FlipFlop name High targets, Seq.fromList . map (,name,High) $ targets) 
pulseOne f@(FlipFlop name High targets) (_, _, Low) =
  (FlipFlop name Low targets, Seq.fromList . map (,name,Low) $ targets) 

pulseOne c@(Conjunction name saved targets) (_, from, pulse) = (newCon, Seq.fromList . map (,name,newPulse) $ targets)
  where newCon = set (inputs . at from) (Just pulse) c :: Module
        allHigh = all (==High) . map snd . M.toList $ newCon^.inputs
        newPulse = if allHigh then Low else High

-- We additionally return the list of strings from the global state
-- that emitted during this pulse
step :: MyRWS ()
step = do
  (values, modules) <- get
  case values of
    Seq.Empty -> return ()  -- No more values to process
    x@(next, from, pulse) :<| xs -> do
      case pulse of
        High -> tell ((0, 1), mempty)
        Low  -> tell ((1, 0), mempty)
        
      watching <- ask
      case from `elem` watching && pulse == High of
        True  -> tell (mempty, [from])
        False -> tell mempty
        
      let nextModule = M.lookup next modules
      case nextModule of
        Nothing -> do
          put $ (xs, modules)
          step
        Just nextM -> do
          let (updatedModule, newPulses) = pulseOne nextM x
          let nextState = (xs >< newPulses, set (at next) (Just updatedModule) modules)
          put nextState
          step

pressButton :: [String] -> Modules -> (Modules, (Int, Int), [String])
pressButton watching modules
  = tuplify $ runRWS step watching (Seq.singleton ("broadcaster", "button", Low), modules)
  where tuplify (_, (_, m), ((lo, hi), seen)) = (m, (getSum lo, getSum hi), seen)

solver1 :: Int -> Modules -> (Int, Int)
solver1 n modules = (stepss, (uncurry (*)) runs)
  where
    go :: Int -> Int -> Map Modules (Modules, (Int, Int)) -> Modules -> (Int, (Int, Int))
    go steps 0 _ _ = (steps, (0, 0))
    go steps n processed modules = case modules `M.lookup` processed of
      Nothing -> go (steps + 1) n (M.insert modules (newmodules, values) processed) modules
        where (newmodules, values, _) = pressButton [] modules
      Just (newmodules, values) -> (0, values) + go steps (n - 1) processed newmodules
    (stepss, runs) = go 0 n M.empty modules

-- The state is the step number, current module state, and the strings left to parse      
presser :: RWS () [(Int, [String])] (Int, Modules, [String]) ()
presser = do
  (n, modules, to_find) <- get
  case to_find of
    [] -> return ()
    _ -> do
      let (newModules, _, seen) = pressButton to_find modules
      case seen of
        []        -> tell mempty
        otherwise -> do
          tell [(n, seen)]
          modify (\(x, y, z) -> (x, y, to_find \\ seen))
      modify (\(x, y, z) -> (x + 1, newModules, z))
      presser


solver2 :: Modules -> Int
solver2 modules = product $ map fst $ indices
  where (_, _, indices) = runRWS presser () (1, modules, rxs)
        rxs = ["jg", "hf", "jm", "rh"]

main :: IO ()
main = do
  -- modules <- parseInput <$> readFile "../inputs/test20.txt"
  -- modules <- parseInput <$> readFile "../inputs/test220.txt"
  modules <- parseInput <$> readFile "../inputs/day20.txt"
  -- print $ solver1 1000 modules
  -- print $ solver2 modules
  print $ solver2 modules
