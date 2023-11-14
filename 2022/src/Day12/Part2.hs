{-# LANGUAGE TupleSections #-}
module Day12.Part2 (solve) where
import Text.Printf (printf)
import Lib (Point, newPoint, getY, getX)
import Data.Char (ord)
import Data.Map (Map, (!), fromList, keys, empty, insertWith, toList, mapKeys)
import qualified Data.Map as M
import Helpers.Graph (dijkstraPath, dijkstra, PathResult, dist, dijkstraPaths)
import Data.List (find, intercalate)
import GHC.Char (chr)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)

toWeightedEdge :: (Point, Char) -> (Point, Char) -> ((Point, Point), Int)
toWeightedEdge (s, 'z') (e, 'E') = ((s, e), 1)
toWeightedEdge (s, 'y') (e, 'E') = ((s, e), 1)
toWeightedEdge (s, 'S') (e, 'a') = ((s, e), 1)
toWeightedEdge (s, 'S') (e, 'b') = ((s, e), 1)
toWeightedEdge (s, _) (e, 'S') = ((s, e), 1)
toWeightedEdge (s, _) (e, 'E') = ((s, e), 10001)
toWeightedEdge (s, c) (e, d) = ((s, e), if d' <= c' + 1 then 1 else 10001)
  where d' = ord d 
        c' = ord c

neighbours :: [Point]
neighbours = map (uncurry newPoint) [(0,1 ), (1, 0), (0, -1), (-1, 0)]

getAllEdges :: Map Point Char -> Point -> [((Point, Point), Int)]
getAllEdges chars point = map (toWeightedEdge pc) next
  where pc = (point, chars ! point)
        next = mapMaybe ((\p -> (p,) <$> M.lookup p chars) . (point +)) neighbours

getNode :: Map Point Char -> Char -> Point
getNode map c =
  case nodeMaybe of
    Just (p, c) -> p
    Nothing -> error "Node not found"
  where nodeMaybe = find (\(p, c') -> c' == c) (toList map)

getNodes :: Map Point Char -> Char -> [Point]
getNodes map c = keys $ M.filter (== c) map



func :: Map Point (PathResult Point) -> Int -> Int -> Char
func m x y = maybe '.' resultToChar (M.lookup (newPoint x y) m)

resultToChar :: PathResult Point -> Char
resultToChar p = chr $ ord '0' + dist p `rem` 10

solve = do
  css <- lines <$> getContents
  let nodes = fromList . concat $ zipWith (\ y cs -> zipWith (\ x c -> (newPoint x y, c)) [0 .. ] cs) [0..] css
  printf "# of Nodes: %d\n" (length nodes)
  let weights = mapKeys (\(x, y) -> (y, x)) $ M.filter (<10000) $ fromList $ concatMap (getAllEdges nodes) $ keys nodes
  printf "# of weights: %d\n" (length weights)
  let edges = foldl (\m (s, e) -> insertWith (++) s [e] m) empty (keys weights)
  let start = getNode nodes 'S'
  let as = getNodes nodes 'a'
  let end = getNode nodes 'E'
  let path = dijkstra weights edges end
  let maxX = maximum $ map getX (keys nodes)
  let maxY = maximum $ map getY (keys nodes)
  let grid = map (\y -> map (\x -> func path x y) [0 .. maxX]) [0..maxY]
  let paths = catMaybes $ dijkstraPaths weights edges (start:as) end
  --printf "Grid: \n%s\n" (intercalate "\n" grid)
  printf "Path length: %d" (minimum $ map (\path -> length path - 1) paths)
