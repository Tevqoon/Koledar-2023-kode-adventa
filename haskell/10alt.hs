import Data.List
import Data.Maybe
import Data.Matrix
import Data.Set qualified as S

type Coord = (Int, Int)
type Board = Matrix Char
type Loop = S.Set Coord


isGood :: Board -> (Coord, [Char]) -> Bool
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

findLoop :: Board -> [Coord]
findLoop m = map fst $ w : takeWhile (\(x, _) -> x /= start) ws
  where w:ws = iterate (getNext m) (start, (0, 0))
        start = findStart m

findStart :: Matrix Char -> Coord
findStart m = fromJust $ fromJust $ find isJust $ toList positions
  where
    positions = mapPos (\c v -> if v == 'S' then Just c else Nothing) m

solver1 m = (length $ findLoop m) `div` 2

mapToLoop :: (Bool -> Char -> a) -> Loop -> (Coord, Char) -> a
mapToLoop f indices (pos, char)
  | pos `S.member` indices = f True  char
  | otherwise        = f False char

ignoreOffLoop :: Loop -> (Coord, Char) -> Char
ignoreOffLoop = mapToLoop (\tf x -> if tf then x else '.')

eraseOffLoop :: Loop -> Board -> Board
eraseOffLoop loop = mapPos $ curry $ (ignoreOffLoop loop)

make3x3 :: Char -> [String]
make3x3 '|' = [" * ", " * ", " * "]
make3x3 '-' = ["   ", "***", "   "]
make3x3 'L' = [" * ", " **", "   "]
make3x3 'J' = [" * ", "** ", "   "]
make3x3 '7' = ["   ", "** ", " * "]
make3x3 'F' = ["   ", " **", " * "]
make3x3 '.' = ["   ", " * ", "   "]
-- Specific to input data, could first replace given pattern
make3x3 'S' = ["   ", "***", "   "]
-- make3x3 'S' = ["   ", " **", " * "] -- test
-- make3x3 'S' = fromLists ["   ", "** ", " * "]

symbolifyLine :: [Char] -> [String]
symbolifyLine = map (foldl1 (++)) . Data.List.transpose . map make3x3 
-- symbolifyLine = map (foldl1 (++)) . map (concatMap id) . map make3x3

symbolify :: Board -> Board
symbolify = fromLists . (symbolifyLine=<<) . toLists

prepare :: Loop -> Board -> Board
prepare loop = symbolify . eraseOffLoop loop

isFillChar :: Char -> Maybe Char -> Bool
isFillChar from maybeChar = maybeChar == Just from
    
floodFill :: Char -> Char -> Board -> Board
floodFill from to board = mapPos (\pos c -> if pos `S.member` to_replace then to else c) board
  where to_replace = aux [(1,1)] S.empty
        aux :: [Coord] -> Loop -> Loop
        aux [] visited = visited
        aux (pos:poss) visited
          | pos `S.member` visited = aux poss visited
          | otherwise              = aux (neighbors pos ++ poss) (S.insert pos visited)
        neighbors (x, y) = filter (isFillChar from . (safeGet1 board)) (candidates)
          where candidates = [(x + dx, y + dy) | (dx, dy) <- [(1,0), (-1, 0), (0, 1), (0, -1)]]

solver2 loop m = count '*' $ toList $ floodFill '*' ' ' $ floodFill ' ' '*' $ (prepare loop m)

count :: Eq a => a -> [a] -> Int
count c = length . filter (==c)
                 
main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day10.txt"
  let mat = fromLists contents
  let loop = S.fromList $ findLoop mat
  print $ solver1 mat
  print $ solver2 loop mat


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
