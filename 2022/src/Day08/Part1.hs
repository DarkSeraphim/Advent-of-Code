module Day08.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Input (readIntChar)
import Data.Map (Map, insertWith, empty, elems)
import Data.List (transpose)
import Lib (Point, newPoint)

data Tree = Tree {point :: Point, height :: Int}

scoreTrees' :: Int -> Map Point Int -> [Tree] -> Map Point Int
scoreTrees' cur map [] = map -- base case
scoreTrees' cur map [_] = map -- last is always visible
scoreTrees' cur map (t1:t2:rest) = insertWith (+) (point t2) v map'
  where cur' = max cur $ height t1
        map' = scoreTrees' (max cur' (height t2)) map (t2:rest) -- continue with the max height so far
        v = if height t2 <= cur' then 1 else 0 -- Add one to the invisibility map

scoreTrees :: [[Tree]] -> Map Point Int
scoreTrees trees = map''''
  where map' = foldl (scoreTrees' 0) empty trees
        map'' = foldl (scoreTrees' 0) map' (map reverse trees)
        map''' = foldl (scoreTrees' 0) map'' (transpose trees)
        map'''' = foldl (scoreTrees' 0) map''' (map reverse $ transpose trees)

solve = do
  heights <- map (map readIntChar) . lines <$> getContents
  let trees = zipWith (\ y row
        -> zipWith (\ x height -> Tree (newPoint x y) height) [0 .. ] row) [0..] heights
  let notSeenBy = scoreTrees trees
  printf "The amount of trees visible is %d" (length $ filter (<4) $ elems notSeenBy)
