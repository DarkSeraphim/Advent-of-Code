module Day17.Part1 (solve) where
import Text.Printf (printf)
import Data.Char (ord)
import Helpers.Parsec (parseGrid)
import Helpers.Point (Point, getX, getY, newPoint, neighbours)
import Helpers.Graph (dijkstra''', computeDistances)
import Data.Set (singleton, empty)
import Data.Map (Map, (!), keys, elems, member)

type Key = (Point, Point, Int)

fromChar :: Char -> Int
fromChar = subtract (ord '0') . ord

weight :: Map Point Int -> (Key, Key) -> Int
weight lossMap (_, (p, _, _)) = lossMap ! p

edges :: Map Point Int -> Key -> [Key]
edges lossMap (p, d, s) = next'
  where score d' = if d' == d then s + 1 else 1
        neighbours' = filter (/= -d) neighbours
        next = map (\d' -> (p + d', d', score d')) neighbours'
        next' = filter (\(p', _, s') -> s' <= 3 && member p' lossMap) next

isPoint :: Point -> Key -> Bool
isPoint a (b, _, _) = a == b

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
  let result = dijkstra''' (weight lossMap) (edges lossMap) empty (singleton (0, startKey, startKey)) (isPoint goal)
  let goals = concatMap (\n' -> map (\d -> (goal, n', d)) [0..3]) neighbours
  let minLoss = minimum $ elems $ computeDistances goals result
  printf "Minimum heat loss was %d\n" minLoss
