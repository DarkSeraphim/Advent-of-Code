module Day12.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Input (toPointMap)
import Helpers.Point (Point, neighbours)
import Data.Set (Set, member, union, singleton, notMember, insert, empty, size, toList)
import Data.Map (Map, findWithDefault, (!), keys)

floodFill :: Map Point Char -> Set Point -> Point -> Set Point
floodFill grid visited p = foldl (floodFill grid) visited' ns
  where ns = filter (`notMember` visited) $ filter same $ map (p +) neighbours
        visited' = foldl (flip insert) visited ns
        c = grid ! p
        same p' = findWithDefault '.' p' grid == c

toRegions :: Map Point Char -> Set Point -> [Point] -> [Set Point]
toRegions    _       _ [ ] = []
toRegions grid visited (p:ps)
  | p `member` visited = toRegions grid visited ps
  | otherwise = region : toRegions grid (region `union` visited) ps
  where region = floodFill grid (singleton p) p

perimeter :: Map Point Char -> Point -> Int
perimeter grid p = 4 - length validNeighbours
  where validNeighbours = filter same $ map (p +) neighbours
        c = grid ! p
        same p' = findWithDefault '.' p' grid == c

price :: Map Point Char -> Set Point -> Int
price grid ps = area * perimeter'
  where area = size ps
        perimeter' = sum $ map (perimeter grid) (toList ps)

solve :: IO ()
solve = do
  grid <- toPointMap id . lines <$> getContents
  let regions = toRegions grid empty $ keys grid
  printf "Regions: %d\n" (sum $ map (price grid) regions)
