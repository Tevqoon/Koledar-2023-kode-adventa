import Data.Char
import Data.List.Split
import Algebra.Lattice
import Numeric.Natural

data Cubes = Cubes { red :: Natural, green :: Natural, blue :: Natural} deriving (Show, Eq)

-- Pointwise addition forms a monoid
instance Semigroup Cubes where
  (Cubes r1 g1 b1) <> (Cubes r2 g2 b2) = Cubes (r1 + r2) (g1 + g2) (b1 + b2)

instance Monoid Cubes where
  mempty = Cubes 0 0 0

-- The componentwise maximum and minimum form a lattice
instance Lattice Cubes where
  (Cubes r1 g1 b1) \/ (Cubes r2 g2 b2) = Cubes (max r1 r2) (max g1 g2) (max b1 b2) -- join
  (Cubes r1 g1 b1) /\ (Cubes r2 g2 b2) = Cubes (min r1 r2) (min g1 g2) (min b1 b2) -- meet
  -- slightly annoying we can't siply define a join semilattice

instance BoundedJoinSemiLattice Cubes where
  bottom = mempty

data Game = Game { id_num :: Natural, moves :: [Cubes] } deriving (Show)

-- From something like " 10 blue"
readColor :: String -> Cubes
readColor pull = case splitOn " " pull of
  (_:value:color:[]) -> case color of
                          "red"   -> Cubes (read value) 0 0
                          "green" -> Cubes 0 (read value) 0
                          "blue"  -> Cubes 0 0 (read value)
                          _       -> error "Bad pattern in color matcher."

-- Each pull's colors converted into Cubes, then summed as cubes into a complete hand
processCubes :: String -> Cubes
processCubes = foldMap readColor . splitOn ","

-- From something like "Game 37"
extractId :: String -> Natural
extractId = read . dropWhile (not . isDigit)

-- Makes a game with the given id and a list of hands for that game
lineToGame :: String -> Game
lineToGame line = Game {id_num = extractId game_id,
                        moves = map processCubes hands}
  where
    game_id : hands = splitOneOf ":;" line

-- Check that all of the pulls in a game are possible - all the pulls must be geq to target
checkGame :: Cubes -> Game -> Bool
checkGame target = all (flip joinLeq target) . moves

-- Sum the ids of possible games - here the explicit target is used
solver1 :: [Game] -> Natural
solver1 = sum . map id_num . filter (checkGame (Cubes 12 13 14))

power :: Cubes -> Natural
power (Cubes r g b) = r * g * b

-- The semilatice structure allows a simple join (pointwise maximum)
-- of all the moves of a game. Then simply sum the powers.
solver2 :: [Game] -> Natural
solver2 = sum . map (power . joins . moves)

readFileLines :: FilePath -> IO [String]
readFileLines = fmap lines . readFile

main :: IO ()
main = do
  contents <- readFileLines "../inputs/day2.txt"
  let games = map lineToGame contents
  print $ solver1 games
  print $ solver2 games
