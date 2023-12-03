import Data.Char
import Data.List.Split
import Algebra.Lattice
import Numeric.Natural

data Cubes = Cubes { red :: Natural, green :: Natural, blue :: Natural } deriving (Show, Eq)

-- The componentwise maximum and minimum form a lattice
instance Lattice Cubes where
  (Cubes r1 g1 b1) \/ (Cubes r2 g2 b2) = Cubes (max r1 r2) (max g1 g2) (max b1 b2) -- join
  (Cubes r1 g1 b1) /\ (Cubes r2 g2 b2) = Cubes (min r1 r2) (min g1 g2) (min b1 b2) -- meet
  -- slightly annoying we can't siply define a join semilattice

instance BoundedJoinSemiLattice Cubes where
  bottom = Cubes 0 0 0

data Game = Game { id_num :: Natural, moves :: Cubes } deriving (Show)

-- From something like " 10 blue"
readColor :: String -> Cubes
readColor pull = case splitOn " " pull of
  (_:value:color:[]) -> case color of
                          "red"   -> Cubes (read value) 0 0
                          "green" -> Cubes 0 (read value) 0
                          "blue"  -> Cubes 0 0 (read value)
                          _       -> error "Bad pattern in color matcher."

-- From something like "Game 37"
extractId :: String -> Natural
extractId = read . dropWhile (not . isDigit)

-- Makes a game with the given id and a hand with the maximal value of each color
lineToGame :: String -> Game
lineToGame line = Game {id_num = extractId game_id,
                        moves = joins $ map readColor hands}
  where
    game_id : hands = splitOneOf ":;," line
    -- This split gives a list with the id in the head and the moves in the tail

tolerance = Cubes 12 13 14

-- Sum the ids of all games with at least enough cubes
solver1 :: [Game] -> Natural
solver1 = sum . map id_num . filter (flip joinLeq tolerance . moves)

power :: Cubes -> Natural
power (Cubes r g b) = r * g * b

-- The semilatice structure allows us to immediately sum the powers
-- of the processed games.
solver2 :: [Game] -> Natural
solver2 = sum . map (power . moves)
 
readFileLines :: FilePath -> IO [String]
readFileLines = fmap lines . readFile

main :: IO ()
main = do
  contents <- readFileLines "../inputs/day2.txt"
  let games = map lineToGame contents
  -- print games
  print $ solver1 games
  print $ solver2 games
