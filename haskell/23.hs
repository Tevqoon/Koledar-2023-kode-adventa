import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Sequence (Seq ((:|>), (:<|)), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Data.Matrix
import Data.NumInstances.Tuple()
import Data.Bifunctor
import qualified Debug.Trace as T

import Data.Maybe
import Data.List
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.RWS

type Coord = (Int, Int)
type Path = [Coord]

type Graph = Map Coord [(Coord, Int)]

parse :: [String] -> (Coord, Coord, Matrix Char)
parse ls = (start, end, fromLists ls)
  where startend = (head ls, last ls)
        (start, end) = bimap (findDot 1) (findDot (length ls)) startend
        findDot n = (n,) . succ . fromJust . findIndex (=='.') 

-- A safe get which takes in a pair instead of two values
safeGet1 :: Matrix a -> Coord -> Maybe a
safeGet1 m (x, y) = safeGet x y m

neighbors :: Matrix Char -> Coord -> [Coord]
neighbors mat pos = filter (safeLook "#") next
  where
    next = case mat ! pos of
      '>' -> singleton (pos + (0, 1)) 
      '<' -> singleton (pos + (0, -1))
      'v' -> singleton (pos + (1, 0)) 
      '^' -> singleton (pos + (-1, 0))
      '.' -> [pos + n | n <- [(0, 1), (0, -1), (1, 0), (-1, 0)]]
    safeLook chars npos = case safeGet1 mat npos of
                           Nothing -> False
                           Just c -> not $ c `elem` chars

insertMax :: Graph -> (Coord, Coord, Int) -> Graph
insertMax graph (from, to, len) =
    M.alter updateEdges to graph
    where
        -- Function to handle insertion or update
        updateEdges Nothing = Just [(from, len)]
        updateEdges (Just edges) = Just $ upsertEdge edges (from, len)

        -- Insert or update the edge in the list
        upsertEdge [] newEdge = [newEdge]
        upsertEdge ((f, l):xs) newEdge@(nf, nl)
            | f == nf   = if nl > l then newEdge : xs else (f, l) : xs
            | otherwise = (f, l) : upsertEdge xs newEdge
                          
-- getJunctions :: (Coord, Coord, Matrix Char) -> Graph
getJunctions (s, e, m) = foldl' insertMax M.empty $ junctions >>= processJunction 
  where
    isJunction _ '#' = Nothing
    isJunction pos _
      | length (neighbors m pos) > 2 = Just pos
      | otherwise = Nothing
      
    junctions = ([s, e] ++) . catMaybes . toList . mapPos isJunction $ m :: [Coord]

    inJunctions j (x:xs) = x `elem` (delete j junctions)
    processJunction j = map (\x -> (head x, last x, length x - 1)) $ bfs m (inJunctions j) j
    
extendPath :: Matrix Char -> Path -> [Path]
extendPath m path@(x:xs) = [ n : path | n <- (neighbors m x) \\ path]

bfs :: Matrix Char -> (Path -> Bool) -> Coord -> [Path]
bfs mat stopCon s = paths
  where
    (_, paths', paths) = runRWS go () ([[s]])
    go :: RWS () ([Path]) ([Path]) ()
    go = do
      paths <- get
      -- case T.trace (show $ map (second length) paths) $ paths of
      case paths of
        [] -> return ()
        otherwise -> do
          let newPaths = paths >>= extendPath mat
          let (finished, going) = partition stopCon newPaths
          tell finished
          put going
          go

type JuncPath = [(Coord, Int)]

junctionPaths :: Graph -> Coord -> Coord -> [JuncPath]
junctionPaths g s e = paths
  where
    (_, paths', paths) = runRWS go () ([[(s, 0)]])
    go :: RWS () ([JuncPath]) ([JuncPath]) ()
    go = do
      paths <- get
      case paths of
        [] -> return ()
        otherwise -> do
          let newPaths = paths >>= extend
          let (finished, going) = partition ((==e) . fst . head) newPaths
          tell finished
          put going
          go
    extend :: JuncPath -> [JuncPath]
    extend path@(x:xs) = [ n : path | n <- removeSeen (neighs x) path]

    removeSeen new path = filter (\(pos, c) -> not $ pos `elem` coords) new
      where coords = map fst path

    neighs :: (Coord, Int) -> [(Coord, Int)]
    neighs (s, n) = g M.! s

sumPath :: JuncPath -> Int
sumPath = sum . map snd
          
solver1 (s, e, m) = maximum $ map sumPath $ junctionPaths (getJunctions (s, e, m)) s e
solver2 (s, e, m) = solver1 (s, e, m')
   where m' = mapPos (\_ char -> if char `elem` "><^v" then '.' else char) m

main :: IO ()
main = do
  contents <- (parse . lines) <$> readFile "../inputs/day23.txt"
  print $ solver1 contents
  print $ length $ getJunctions contents
  print $ solver2 contents
  print 0

test = parse ["#.#####################",
              "#.......#########...###",
              "#######.#########.#.###",
              "###.....#.>.>.###.#.###",
              "###v#####.#v#.###.#.###",
              "###.>...#.#.#.....#...#",
              "###v###.#.#.#########.#",
              "###...#.#.#.......#...#",
              "#####.#.#.#######.#.###",
              "#.....#.#.#.......#...#",
              "#.#####.#.#.#########v#",
              "#.#...#...#...###...>.#",
              "#.#.#v#######v###.###v#",
              "#...#.>.#...>.>.#.###.#",
              "#####v#.#.###v#.#.###.#",
              "#.....#...#...#.#.#...#",
              "#.#########.###.#.#.###",
              "#...###...#...#...#.###",
              "###.###.#.###v#####v###",
              "#...#...#.#.>.>.#.>.###",
              "#.###.###.#.###.#.#v###",
              "#.....###...###...#...#",
              "#####################.#"]

test2 = parse ["#.##",
               "v.>v",
               ">v<.",
               "#.##"]

test3 = parse ["#.##",
               "....",
               "##.#"]        

(s, e, m) = test
m' = mapPos (\_ char -> if char `elem` "><^v" then '.' else char) m
