module Day10.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Input (toPointMap)
import Helpers.Map ((!?))
import Data.Map (Map, (!), keys)
import qualified Data.Map as M
import Helpers.Point (Point, neighbours)

digitToInt :: Char -> Int
digitToInt c = read [c]

isGoodNeighbour :: Map Point Int -> Point -> Point -> Bool
isGoodNeighbour grid start neighbour = hn - hs == 1
  where hs = grid ! start
        hn = grid !? (neighbour, maxBound)

solveForPoint :: Map Point Int -> Point -> Int
solveForPoint grid start
  | grid ! start == 9 = 1
  | otherwise = sum $ map (solveForPoint grid) edges
  where edges = filter (isGoodNeighbour grid start) $ map (start +) neighbours

solve :: IO ()
solve = do
  heightMap <- toPointMap digitToInt . lines <$> getContents
  let start = keys $ M.filter (== 0) heightMap
  let result = sum $ map (solveForPoint heightMap) start
  printf "Total trail score is %d\n" result

