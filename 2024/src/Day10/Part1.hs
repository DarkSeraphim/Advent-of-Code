module Day10.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Input (toPointMap)
import Data.Map (Map, (!), keys)
import qualified Data.Map as M
import Helpers.Point (Point, neighbours)
import Data.Set (singleton, empty)
import Helpers.Graph (dijkstra''')
import Helpers.Map ((!?))

digitToInt :: Char -> Int
digitToInt c = read [c]

isGoodNeighbour :: Map Point Int -> Point -> Point -> Bool
isGoodNeighbour grid start neighbour = hn - hs == 1
  where hs = grid ! start
        hn =  grid !? (neighbour, maxBound)

solveForPoint :: Map Point Int -> Point -> Int
solveForPoint grid start = length $ filter (== 9) $ map (grid !) $ keys paths
  where wfunc = const 1
        efunc p = filter (isGoodNeighbour grid p) $ map (p +) neighbours
        paths = dijkstra''' wfunc efunc empty (singleton (0, start, start)) (const False)

solve :: IO ()
solve = do
  heightMap <- toPointMap digitToInt . lines <$> getContents
  let start = keys $ M.filter (== 0) heightMap
  let result = sum $ map (solveForPoint heightMap) start
  printf "Total trail score is %d\n" result
