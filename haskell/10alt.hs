import Data.List
import Data.Maybe
import Data.Array
import Data.Set qualified as S

type Coord = (Int, Int)
type Board = Array Coord Char
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

fromList :: [a] -> Array Coord a
fromList l = listArray ((1, 1), (up, up)) l
  where up = round $ sqrt $ fromIntegral $ length l

fromLists :: [[a]] -> Array Coord a
fromLists l = listArray ((1, 1), (length l, length (head l))) (concat l)

toList :: Array Coord a -> [a]
toList = elems

toLists :: Board -> [[Char]]
toLists arr = 
  [[arr ! (i, j) | j <- [minJ..maxJ]] | i <- [minI..maxI]]
  where
    is = bounds arr
    minI = fst $ fst is
    maxI = fst $ snd is
    minJ = snd $ fst is
    maxJ = snd $ snd is

mapPos :: (Coord -> Char -> Char) -> Board -> Board
mapPos f arr = array (bounds arr) [(coord, f coord (arr ! coord)) | coord <- indices arr]

-- A safe get which takes in a pair instead of two values
safeGet1 :: Board -> Coord -> Maybe Char
safeGet1 m (ix, iy) = case i1x <= ix && ix <= i2x && i1y <= iy && iy <= i2y of
  False -> Nothing
  True  -> Just (m ! (ix, iy))
  where ((i1x, i1y), (i2x, i2y)) = bounds m
  

findLoop1 :: Board -> [(Coord, Char)]
findLoop1 m = map (\(x,y) -> ((x, y), m ! (x, y))) $ map fst $ take cutoff walking
  where walking = iterate (getNext m) (start, start)
        cutoff = succ $ fromJust $ findIndex (\((x, y), _) -> (x, y) == start) (tail walking)
        start = findStart m

findLoop :: Board -> [Coord]
findLoop m = map fst $ w : takeWhile (\(x, _) -> x /= start) ws
  where w:ws = iterate (getNext m) (start, (0, 0))
        start = findStart m

findStart :: Board -> Coord
findStart m = fst $ fromJust $ find (\(i, v) -> v == 'S') $ assocs m

solver1 m = (length $ findLoop m) `div` 2

mapToLoop :: (Bool -> Char -> a) -> Loop -> (Coord, Char) -> a
mapToLoop f indices (pos, char)
  | pos `S.member` indices = f True  char
  | otherwise        = f False char

ignoreOffLoop :: Loop -> (Coord, Char) -> Char
ignoreOffLoop = mapToLoop (\tf x -> if tf then x else '.')

eraseOffLoop :: Loop -> Board -> Board
eraseOffLoop loop = mapPos (\pos char -> ignoreOffLoop loop (pos, char))

-- make3x3 :: Char -> [String]
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

symbolify :: Board -> Board
symbolify = fromLists . (symbolifyLine=<<) . toLists

prepare :: Loop -> Board -> Board
prepare loop = symbolify . eraseOffLoop loop

isFillChar :: Char -> Maybe Char -> Bool
isFillChar from maybeChar
  | maybeChar == Just from = True
  | otherwise = False
    
-- neighbors :: Char -> Board -> Coord -> [Coord]
-- neighbors from board (x, y) = matching
--   where candidates = [(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], (dx, dy) /= (0, 0)]
--         matching = filter (isFillChar from board) candidates

floodFill :: Char -> Char -> Board -> (Board, Int)
floodFill from to board = (board // [(i, to) | i <- S.toList to_replace], counter)
  -- mapPos (\pos c -> if pos `S.member` to_replace then to else c) board
  where (to_replace, counter) = aux [(1,1)] S.empty 0
        aux :: [Coord] -> Loop -> Int -> (Loop, Int)
        aux [] visited counter = (visited, counter)
        aux (pos:poss) visited counter
          | pos `S.member` visited = aux poss visited counter
          | otherwise              = aux ((neighbors pos) ++ poss) (S.insert pos visited) (succ counter)
        neighbors (x, y) = filter (isFillChar from . (safeGet1 board)) candidates
          where candidates =[(x + dx, y + dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]

solver2 loop m = count '*' $ toList $ fst $ floodFill '*' ' ' $ fst $ floodFill ' ' '*' $ (prepare loop m)

count c = length . filter (==c)
                 
main :: IO ()
main = do
  contents <- lines <$> readFile "../inputs/day10.txt"
  let mat = fromLists contents
  let loop = S.fromList $ findLoop mat
  print $ solver1 mat
  let loop = S.fromList $ findLoop mat
  -- let symb = prepare loop mat
  --let (ff1, c1) = floodFill ' ' '*' symb
  -- let (ff2, c2) = floodFill '*' ' ' ff1
  -- print $ count '*' $ toList $ symb
  -- putStrLn ""
  -- print $ 9 * (length contents) ^ 2
  -- print c1
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
