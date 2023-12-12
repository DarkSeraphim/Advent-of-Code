{-# LANGUAGE TupleSections #-}
module Day11.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Helpers.Graph (dijkstra', computeDistances)
import Helpers.Point (getX, getY, newPoint, Point, neighbours)
import Data.Map (keys, (!), Map, mapWithKey, member)
import Data.Set (Set, member)
import qualified Data.List
import qualified Data.Map as M
import qualified Data.Set as S

data SpaceObject = Galaxy | Empty deriving (Eq)

fromChar :: Char -> SpaceObject
fromChar '#' = Galaxy
fromChar  _  = Empty

getMinMaxBy :: Ord b => (a -> b) -> [a] -> (b, b)
getMinMaxBy f as = (minimum bs, maximum bs)
  where bs = map f as

interval :: Integral a => (a, a) -> [a]
interval (x, y) = [x..y]

getEmpties :: Map Point SpaceObject -> (Int -> Int -> Point) -> (Int, Int) -> (Int, Int) -> [Int]
getEmpties space f minMaxX minMaxY = Data.List.filter (\x -> all (\y -> Empty == space ! f x y) (interval minMaxY)) (interval minMaxX)

enterWeight :: Set Int -> Set Int -> (Point, Point) -> Int
enterWeight bigX bigY (a, b)
  | dirIsX    = if getX b `Data.Set.member` bigX then 2 else 1
  | otherwise = if getY b `Data.Set.member` bigY then 2 else 1
  where dirIsX = getX a /= getX b

interconnected :: Map Point a -> Map Point [Point]
interconnected grid = mapWithKey f grid
  where f k _ = Data.List.filter (`Data.Map.member` grid) $ map (k +) neighbours

computeAllPairs :: [a] -> [(a, a)]
computeAllPairs [] = []
computeAllPairs [_] = []
computeAllPairs (a:as) = map (a,) as ++ computeAllPairs as

solve :: IO ()
solve = do
  space <- parseGrid fromChar
  let minMaxX = getMinMaxBy getX (keys space)
  let minMaxY = getMinMaxBy getY (keys space)
  let bigX = S.fromList $ getEmpties space newPoint minMaxX minMaxY
  let bigY = S.fromList $ getEmpties space (flip newPoint) minMaxY minMaxX
  let galaxies = keys $ M.filter (==Galaxy) space
  let graphs = M.fromList $ map (\p -> (p, dijkstra' (enterWeight bigX bigY) (interconnected space) p (const False))) galaxies
  let completeGraph = M.map (computeDistances galaxies) graphs
  let allGalaxies = computeAllPairs galaxies
  let allDistances = map (\(a, b) -> (completeGraph ! a) ! b) allGalaxies

  printf "The total sum of distances is %d\n" (sum allDistances)
