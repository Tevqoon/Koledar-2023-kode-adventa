import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Matrix
import Data.NumInstances.Tuple()
import Data.Bifunctor
import qualified Debug.Trace as T

import Data.Array.Unboxed (bounds, UArray)

import Data.Maybe
import Data.List
import Control.Monad.Reader
import Control.Monad.RWS

type Coord = (Int, Int)
type Path = (Coord, Set Coord)

parse :: [String] -> (Coord, Coord, Matrix Char)
parse ls = (start, end, fromLists ls)
  where startend = (head ls, last ls)
        (start, end) = bimap (findDot 1) (findDot (length ls)) startend
        findDot n = (n,) . succ . fromJust . findIndex (=='.') 

-- A safe get which takes in a pair instead of two values
safeGet1 :: Matrix a -> Coord -> Maybe a
safeGet1 m (x, y) = safeGet x y m

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

neighbors :: Matrix Char -> Coord -> [(Coord, Set Coord)]
neighbors mat pos =
  case mat ! pos of
    '>' -> let npos = (pos + (0, 1))  in singleton (npos, S.singleton npos)
    '<' -> let npos = (pos + (0, -1)) in singleton (npos, S.singleton npos)
    'v' -> let npos = (pos + (1, 0))  in singleton (npos, S.singleton npos)
    '^' -> let npos = (pos + (-1, 0)) in singleton (npos, S.singleton npos)
    'j' -> [(npos, S.singleton npos) | dx <- [(0, 1), (0, -1), (1, 0), (-1, 0)], let npos = pos + dx, safeLook "j#" npos] 
    '.' -> [npath | npath@(_, n) <- [potDown, potUp, potLeft, potRight], not . S.null $ n]
  where
    potDown  = let npath = takeWhileOneMore (safeLook "v<>j") $ takeWhile (safeLook "#^") [pos + (n, n) * (1, 0) | n <- [1..]]  in (last npath, S.fromList npath)
    potUp    = let npath = takeWhileOneMore (safeLook "^<>j") $ takeWhile (safeLook "#v") [pos + (n, n) * (-1, 0) | n <- [1..]] in (last npath, S.fromList npath)
    potLeft  = let npath = takeWhileOneMore (safeLook "^v<j") $ takeWhile (safeLook "#>") [pos + (n, n) * (0, -1) | n <- [1..]] in (last npath, S.fromList npath)
    potRight = let npath = takeWhileOneMore (safeLook "^v>j") $ takeWhile (safeLook "#<") [pos + (n, n) * (0, 1) | n <- [1..]]  in (last npath, S.fromList npath)
    
    safeLook chars npos = case safeGet1 mat npos of
                           Nothing -> False
                           Just c -> not $ c `elem` chars

extendPath :: Matrix Char -> Path -> [Path]
extendPath m (current, seen) = [ (next, S.union seen new_elements) | (next, new_elements) <- map (second (S.\\ seen)) (neighbors m current), not . S.null $ new_elements ]

-- getPaths :: Matrix Char -> Coord -> Coord
getPaths m s e = paths
  where
    (_, paths', paths) = runRWS go (m, e) (singleton (s, S.singleton s))
    go :: RWS (Matrix Char, Coord) ([Path]) ([Path]) ()
    go = do
      (mat, stop) <- ask
      paths <- get
      -- case T.trace (show $ map (second length) paths) $ paths of
      case paths of
        [] -> return ()
        otherwise -> do
          let newPaths = paths >>= extendPath mat
          let (finished, going) = partition (snd . second (S.member stop)) newPaths
          tell finished
          put newPaths
          go
          
solver1 (s, e, m) = maximum $ map (pred . snd . (second length)) $ getPaths m s e
solver2 (s, e, m) = solver1 (s, e, m')
  where m' = mapPos (\_ char -> if char `elem` "><^v" then 'j' else char) m

main :: IO ()
main = do
  contents <- (parse . lines) <$> readFile "../inputs/day23.txt"
  -- print $ solver1 contents
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

(s, e, m) = test
