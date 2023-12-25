import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Matrix (Matrix)
import qualified Data.Matrix as A
import Data.NumInstances.Tuple()
import Data.List.Split
import Data.Bifunctor

import Data.Array.Unboxed (bounds, UArray)

import qualified Data.Graph as G

import Data.Maybe
import Data.List

type Graphy =  Map String (Set String)

parseLine :: String -> (String, (Set String))
parseLine = bimap id (S.fromList . neighs) . nameNeighs
  where nameNeighs = tuple2 . splitOn ": "
        neighs = splitOn " "

        tuple2 = \[x,y] -> (x, y)
        dup = \(x, y) -> (x, x, y)

-- parse :: [String] -> [(String, String, [String])]
parse = addDisconnected . M.fromList . map parseLine

addDisconnected :: Graphy -> Graphy
addDisconnected m = foldl (\tab key -> insertIntoAll tab (M.lookup key m) key) m allNodes
  where keys = S.fromList $ M.keys m
        vals = S.unions $ S.map (m M.!) keys
        allNodes = S.union keys vals

replace :: Int -> a -> [a] -> [a]
replace i x xs = xs1 ++ [x] ++ (tail xs2)
  where (xs1, xs2) = splitAt i xs

adder :: String -> Maybe (Set String) -> Maybe (Set String)
adder key Nothing = Just $ S.singleton key
adder key (Just keys) = Just $ S.union (S.singleton key) keys

insertList :: Graphy -> String -> String -> Graphy
insertList table key val = M.alter (adder val) key table

insertIntoAll :: Graphy -> Maybe (Set String) -> String -> Graphy
insertIntoAll table Nothing val = table
insertIntoAll table (Just keys) val = foldl (\tab key -> insertList tab key val) table keys


pairs :: [a] -> [(a, a)]
pairs l = [(x, y) | (x:ys) <- tails l, y <- ys]

triples :: [a] -> [(a, a, a)]
triples l = [(x, y, z) | (x:ys) <- tails l, (y:zs) <- tails ys, z <- zs]

mapToGraphList m = map (\(x, y) -> (x, x, S.toList y)) $ M.assocs m

-- solver1 :: Graphy -> Int
solver1 g = find (\x -> (==2) $ length $ x) withoutTriples
  where keys = M.keys g
        trips = triples keys
        removeTriples m (a, b, c) = M.delete a $ M.delete b $ M.delete c $ m
        withoutTriples = map (G.stronglyConnComp . mapToGraphList . removeTriples g) trips 

main :: IO ()
main = do
  contents <- (parse . lines) <$> readFile "../inputs/day23.txt"

  print 0


test = parse ["jqt: rhn xhk nvd",
              "rsh: frs pzl lsr",
              "xhk: hfx",
              "cmg: qnr nvd lhk bvb",
              "rhn: xhk bvb hfx",
              "bvb: xhk hfx",
              "pzl: lsr hfx nvd",
              "qnr: nvd",
              "ntq: jqt hfx bvb xhk",
              "nvd: lhk",
              "lsr: lhk",
              "rzs: qnr cmg lsr rsh",
              "frs: qnr lhk lsr"]
