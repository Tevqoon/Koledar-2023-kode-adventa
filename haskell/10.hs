import Data.List
import Data.Matrix
import Data.Maybe
import Rainbow
import Data.Text qualified as T
import Data.Set qualified as S

type Coord = (Int, Int)
type Board = Matrix Char
type Pipe = Char

isGood :: Board -> (Coord, [Pipe]) -> Bool
isGood m (c, a) = case (safeGet1 m c) of
  Nothing -> False
  Just p  -> p `elem` a

getNext :: Board -> (Coord, Coord) -> (Coord, Coord)
getNext m ((x, y), previous) = (head possible, (x, y))
  where possible = case m ! (x, y) of
          '|' -> [(x + 1, y), (x - 1, y)] \\ [previous]
          '-' -> [(x, y + 1), (x, y - 1)] \\ [previous]
          'L' -> [(x, y + 1), (x - 1, y)] \\ [previous]
          'J' -> [(x - 1, y), (x, y - 1)] \\ [previous]
          '7' -> [(x + 1, y), (x, y - 1)] \\ [previous]
          'F' -> [(x + 1, y), (x, y + 1)] \\ [previous]
          '.' -> undefined
          'S' -> map fst $ filter (isGood m) [((x, y - 1), ['L', '-', 'F']),
                                              ((x, y + 1), ['J', '-', '7']),
                                              ((x + 1, y), ['|', 'J', 'L']),
                                              ((x - 1, y), ['|', '7', 'F'])]

-- A safe get which takes in a pair instead of two values
safeGet1 :: Matrix a -> Coord -> Maybe a
safeGet1 m (x, y) = safeGet x y m

findLoop1 :: Board -> [(Coord, Pipe)]
findLoop1 m = map (\(x,y) -> ((x, y), m ! (x, y))) $ map fst $ take cutoff walking
  where walking = iterate (getNext m) (start, start)
        cutoff = succ $ fromJust $ findIndex (\((x, y), _) -> (x, y) == start) (tail walking)
        start = findStart m

findLoop :: Board -> [(Coord, Pipe)]
findLoop m = map (\(x,y) -> ((x, y), m ! (x, y))) $ map fst $ (head walking) : takeWhile (\(pos1, pos2) -> pos1 /= start) (tail walking)
  where walking = iterate (getNext m) (start, start)
        start = findStart m

findStart :: Matrix Char -> Coord
findStart m = fromJust $ fromJust $ find isJust $ toList positions
  where
    positions = mapPos (\c v -> if v == 'S' then Just c else Nothing) m

solver1 m = (length $ findLoop m) `div` 2

mapToLoop :: (Bool -> Char -> a) -> [Coord] -> (Coord, Char) -> a
mapToLoop f indices (pos, char)
  | elem pos indices = f True  char
  | otherwise        = f False char

ignoreOffLoop :: [Coord] -> (Coord, Char) -> Char
ignoreOffLoop = mapToLoop (\tf x -> if tf then x else '.')

eraseOffLoop :: [Coord] -> Board -> Board
eraseOffLoop loop = mapPos (\pos char -> ignoreOffLoop loop (pos, char))

make3x3 :: Pipe -> Board
make3x3 '|' = fromLists [" * ", " * ", " * "]
make3x3 '-' = fromLists ["   ", "***", "   "]
make3x3 'L' = fromLists [" * ", " **", "   "]
make3x3 'J' = fromLists [" * ", "** ", "   "]
make3x3 '7' = fromLists ["   ", "** ", " * "]
make3x3 'F' = fromLists ["   ", " **", " * "]
make3x3 '.' = fromLists ["   ", " * ", "   "]
-- Specific to input data, could first replace given pattern
make3x3 'S' = fromLists ["   ", "***", "   "]
-- make3x3 'S' = fromLists ["   ", " **", " * "]
-- make3x3 'S' = fromLists ["   ", "** ", " * "]

expandify :: Board -> Matrix Board
expandify = mapPos (\_ c -> make3x3 c)

concatMatrix :: Matrix Board -> Board
concatMatrix = foldl1 (<->) . map (foldl1 (<|>)) . toLists

symbolify :: Board -> Board
symbolify board = concatMatrix $ expandify $ eraseOffLoop (map fst (findLoop board)) board

isFillChar :: Char -> Board -> Coord -> Bool
isFillChar from board pos
  | maybeChar == Just from = True
  | otherwise = False
    where maybeChar = safeGet1 board pos
    
neighbors :: Char -> Board -> Coord -> [Coord]
neighbors from board (x, y) = matching
  where candidates = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]
        matching = filter (isFillChar from board) candidates

floodFill :: Char -> Char -> Board -> Board
floodFill from to board = mapPos (\pos c -> if pos `S.member` to_replace then to else c) board
  where to_replace = aux [(1,1)] S.empty
        aux [] visited = visited
        aux (pos:poss) visited
          | pos `S.member` visited = aux poss visited
          | otherwise = aux ((neighbors from board pos) ++ poss)  (S.insert pos visited)

solver2 m = length $ filter (=='*') $ toList $ floodFill '*' ' ' $ floodFill ' ' '*' $ (symbolify m)
          
main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day10.txt"
  let mat = fromLists contents
  print $ solver1 mat
  let symb = symbolify mat
  let ff1 = floodFill ' ' '*' symb
  let ff2 = floodFill '*' ' ' ff1
  print $ length $ filter (=='*') $ toList $ ff2

-- Function to colorize a character based on its position
colorizeChar True  char = fore black $ chunk $ T.singleton char
colorizeChar False char = chunk $ T.singleton '●'

colorizeLoop = mapToLoop colorizeChar

-- Function to print the entire matrix with colored elements
printColoredMatrix :: [Coord] -> Board -> IO ()
printColoredMatrix indices matrix =
  mapM_ putChunksLn $ toLists $ mapPos (\pos char -> colorizeLoop indices (pos, (makePipe char))) matrix

-- Aux function to replace the pipe characters
makePipe :: Pipe -> Pipe
makePipe '|' = '┃'
makePipe '-' = '━'
makePipe 'L' = '┗'
makePipe 'J' = '┛'
makePipe '7' = '┓'
makePipe 'F' = '┏'
makePipe '.' = '●'
makePipe 'S' = '━'
makePipe x = x


test = fromLists ["-L|F7",
                  "7S-7|",
                  "L|7||",
                  "-L-J|",
                  "L|-JF"]

test2 = fromLists ["FF7FSF7F7F7F7F7F---7",
                   "L|LJ||||||||||||F--J",
                   "FL-7LJLJ||||||LJL-77",
                   "F--JF--7||LJLJ7F7FJ-",
                   "L---JF-JLJ.||-FJLJJ7",
                   "|F|F-JF---7F7-L7L|7|",
                   "|FFJF7L7F-JF7|JL---7",
                   "7-L-JL7||F7|L7F-7F7|",
                   "L.L7LFJ|||||FJL7||LJ",
                   "L7JLJL-JLJLJL--JLJ.L"]

test3 = fromLists ["...........",
                   ".S-------7.",
                   ".|F-----7|.",
                   ".||.....||.",
                   ".||.....||.",
                   ".|L-7.F-J|.",
                   ".|..|.|..|.",
                   ".L--J.L--J.",
                   "..........."]
