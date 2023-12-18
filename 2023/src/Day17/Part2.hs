module Day17.Part2 (solve) where
import Text.Printf (printf)
import Data.Char (ord)
import Helpers.Parsec (parseGrid)
import Helpers.Point (Point, getX, getY, newPoint, neighbours)
import Data.Set (singleton, empty)
import Data.Map (Map, (!), keys, elems, member)
import Helpers.Graph (dijkstra''', computeDistances)

type Key = (Point, Point, Int)

fromChar :: Char -> Int
fromChar = subtract (ord '0') . ord

weight :: Map Point Int -> (Key, Key) -> Int
weight lossMap (_, (p, _, _)) = lossMap ! p

edges :: Map Point Int -> Point -> Key -> [Key]
edges lossMap goal (p, d, s)
  | p == goal = []
  | s > 0 && s < 4    = [(nextForced, d, s + 1) | nextForced `member` lossMap]
  | otherwise = next'
  where nextForced = p + d
        score d' = if d' == d then s + 1 else 1
        neighbours' = filter (/= -d) neighbours
        next = map (\d' -> (p + d', d', score d')) neighbours'
        next' = filter (\(p', _, s') -> s' <= 10 && member p' lossMap) next

isPointAtGoodDist :: Point -> Key -> Bool
isPointAtGoodDist a (b, _, s) = a == b && s >= 4 && s <= 10

solve :: IO ()
solve = do
  lossMap <- parseGrid fromChar

  let xs = map getX $ keys lossMap
  let ys = map getY $ keys lossMap
  let minX = minimum xs
  let maxX = maximum xs
  let minY = minimum ys
  let maxY = maximum ys


  let goal = newPoint maxX maxY
  let start = newPoint minX minY
  let startKey = (start, start, 0)
  let result = dijkstra''' (weight lossMap) (edges lossMap goal) empty (singleton (0, startKey, startKey)) (isPointAtGoodDist goal)
  let goals = concatMap (\n' -> map (\d -> (goal, n', d)) [4..10]) neighbours
  let minLoss = minimum $ elems $ computeDistances goals result
  printf "Minimum heat loss was %d\n" minLoss
