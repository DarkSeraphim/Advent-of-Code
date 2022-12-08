module Day08.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Input (readIntChar)
import Data.Map (Map, fromList, member, (!), keys)
import Lib (Point, newPoint)

data Tree = Tree {point :: Point, height :: Int}

measureDistance :: Int -> Map Point Int -> Point -> Point -> Int
measureDistance maxHeight heights pos delta =
  if member pos' heights then 1 +
    if (heights ! pos') < maxHeight then other
    else 0
  else 0
  where pos' = pos + delta
        other = measureDistance maxHeight heights pos' delta

directions :: [Point]
directions = map (uncurry newPoint) [(1, 0), (0, -1), (-1, 0), (0, 1)]

scoreTree :: Map Point Int -> Point -> Int
scoreTree heights start = product dists
  where maxHeight = heights ! start
        dists = map (measureDistance maxHeight heights start) directions

solve = do
  heights <- map (map readIntChar) . lines <$> getContents
  let trees = concat $ zipWith (\ y row
        -> zipWith (\ x height -> Tree (newPoint x y) height) [0 .. ] row) [0..] heights
  let treeMap = fromList $ map (\x -> (point x, height x)) trees
  printf "The amount of trees visible is %d" (maximum $ map (scoreTree treeMap) (keys treeMap))
