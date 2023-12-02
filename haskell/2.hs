import Data.Char
import Data.List.Split
import qualified Data.Text as T

data Cubes = Cubes { red :: Integer, green :: Integer, blue :: Integer} deriving (Show, Eq)

instance Semigroup Cubes where
  (Cubes r1 g1 b1) <> (Cubes r2 g2 b2) = Cubes (r1 + r2) (g1 + g2) (b1 + b2)

instance Monoid Cubes where
  mempty = Cubes 0 0 0

instance Ord Cubes where
  (Cubes r1 g1 b1) <= (Cubes r2 g2 b2) = r1 <= r2 && g1 <= g2 && b1 <= b2

  -- Note: This definition means that max needs not be equal to
  -- one of the old elements. This is the behavior we want here,
  -- but does not match the expected interactions in Ord.
  -- Is that a good thing? Idk.
  max (Cubes r1 g1 b1) (Cubes r2 g2 b2) =
    Cubes (max r1 r2) (max g1 g2) (max b1 b2)

data Game = Game { id_num :: Integer, moves :: [Cubes] } deriving (Show)

-- From something like "Game 37"
extractId :: String -> Integer
extractId = read . dropWhile (not . isDigit)

readColor :: String -> Cubes
readColor pull = case splitOn " " pull of 
                   (_:x:"red":[])   -> Cubes (read x) 0 0
                   (_:x:"green":[]) -> Cubes 0 (read x) 0
                   (_:x:"blue":[])  -> Cubes 0 0 (read x)
                   _               -> error "Bad pattern in color matcher."

-- Each pull's colors converted into Cubes, then summed as cubes
processCubes :: String -> Cubes
processCubes = foldMap readColor . splitOn ","

-- Makes a game with the given id and a list of hands for that game
lineToGame :: String -> Game
lineToGame line = Game {id_num = extractId game_id, moves = map processCubes hands}
  where
    game_id : hands = splitOneOf ":;" line

-- Check that all of the pulls in a game are possible
checkGame :: Cubes -> Game -> Bool
checkGame target = all (<= target) . moves

-- Sum the ids of possible games
solver1 :: [Game] -> Integer
solver1  = sum . map id_num . filter (checkGame (Cubes 12 13 14))

power :: Cubes -> Integer
power (Cubes r g b) = r * g * b

-- Taking advantage of the ill-defined Ord to fold each game into
-- componentwise maxima
count2 :: Game -> Integer
count2 = power . foldr max mempty . moves

solver2 :: [Game] -> Integer
solver2 = sum . map count2

main :: IO ()
main = do
  contents <- readFile "../inputs/day2.txt"
  let games = map lineToGame $ lines contents
  print $ solver1 games
  print $ solver2 games
