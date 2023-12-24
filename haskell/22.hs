{-# LANGUAGE TemplateHaskell #-}
import Data.Set (Set)
import qualified Data.Set as S
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Ix
import Data.List
import Data.List.Split
import Data.Bifunctor
import Data.Sequence (Seq ((:|>), (:<|)))
import Data.Maybe
import qualified Data.Sequence as Seq
import Debug.Trace
import Data.Foldable (toList)

import Control.Lens

import Control.Monad.State
import Control.Monad.RWS

type Coord = (Int, Int, Int)
data Block = Block {_coords :: Set Coord, _parents :: [Int], _children :: [Int]}
makeLenses ''Block
type Blocks = IntMap Block

parseLine :: String -> Block
parseLine line = Block (S.fromList . range $ bimap coords coords bounds) [] []
  where
    bounds = tuple2 . splitOn "~" $ line
    coords = tuple3 . map read . splitOn ","
    
    tuple3 = \[x,y,z] -> (x, y, z)
    tuple2 = \[x,y] -> (x, y)

parse :: [String] -> Blocks
parse ls = M.fromList (zip [1..] $ map parseLine ls)

-- If a block has a stable underneath, it moves down one.
-- If a block has a falling underneath, it goes to the back of the line.
-- If a block has nothing underneath, it moves down one and keeps falling.

canMove :: Block -> Blocks -> (Maybe Block, Blocks)
canMove block blocks = undefined
  where lower = S.map (\(x, y, z) -> (x, y, z - 1)) $ block^.coords
        above_zero = all (\(x, y, z) -> z > 0) lower
        above_all = S.null $ S.intersection lower all_points
        
        lower_state = if above_zero && above_all then Just lower else Nothing

-- Stabilize stabilizes all the blocks.
-- Each block keeps track of its parents, so each time a block gets stabilized, all of the
-- blocks below it get updated.

-- Then we can disintegrate any block whose children are more than we have.
stabilize :: State (Seq Block, Blocks) ()
stabilize = do
  (falling, stabilized) <- get
  case falling of
    Seq.Empty -> return ()
    (x :<| xs) -> case canMove x (stabilized) of
      Nothing -> do
        put (xs, S.union (S.singleton x) stabilized)
        stabilize
      Just _ -> case canMove x (S.fromList . toList $ xs) of
        Nothing -> do
          put (xs Seq.|> x, stabilized)
          stabilize
        Just lower -> do
          put (xs Seq.|> lower, stabilized)
          stabilize

stabilizeWrapper :: [Block] -> [Block]
stabilizeWrapper falling = S.toList $ (snd . snd) . runState stabilize $ (Seq.fromList falling, S.empty)

-- Read : All of the blocks we have
-- Write : Accumulating the blocks that can be disintegrated safely
-- State : The blocks we still have to check
canRemove :: RWS (Set Block) ([Block]) ([Block]) ()
canRemove = do
  next <- get
  case next of
    [] -> return ()
    (block:xs) -> do
      blocks <- ask
      let without = S.delete block blocks
      let above = S.toList $ blocksAbove block without
      let moves = mapMaybe (\x -> canMove x (S.delete x without)) above

      if null moves
        then tell [block]
        else tell []
      put xs
      canRemove

canRemoveWrapper :: [Block] -> [Block]
canRemoveWrapper stabilized = w
  where (a, s, w) = runRWS canRemove (S.fromList stabilized) (stabilized)

blocksAbove :: Block -> Set Block -> Set Block
blocksAbove block blocks = S.filter (\b -> not $ S.null (S.intersection above b)) blocks
  where above = S.map (\(x,y,z) -> (x, y, z + 1)) block

solver1 = length . canRemoveWrapper . stabilizeWrapper   

main :: IO ()
main = do
  contents <- (parse . lines) <$> readFile "../inputs/day22.hs"
  print 0

test = parse ["1,0,1~1,2,1",
                  "0,0,2~2,0,2",
                  "0,2,3~2,2,3",
                  "0,0,4~0,2,4",
                  "2,0,5~2,2,5",
                  "0,1,6~2,1,6",
                  "1,1,8~1,1,9"]
