{-# LANGUAGE TupleSections #-}
module Day24.Part2 (solve) where
import Text.Printf (printf)
import Lib (newPoint, Point, getY, getX, neighbours)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Map (Map)
import Helpers.Graph (bfsPathCond)
import Debug.Trace (trace)
import Data.List (intercalate)

data Dir = U | D | L | R deriving (Eq, Ord)
data Cell = Empty | Wall | Blizzard Dir deriving (Eq, Ord)
type Node = (Int, Point)

dirToChar U = '^'
dirToChar D = 'v'
dirToChar L = '<'
dirToChar R = '>'

charToDir '^' = U
charToDir 'v' = D
charToDir '<' = L
charToDir '>' = R
charToDir _ = error "Invalid direction"

dirToDelta :: Dir -> Point
dirToDelta U = newPoint 0 (-1)
dirToDelta D = newPoint 0 1
dirToDelta L = newPoint (-1) 0
dirToDelta R = newPoint 1 0

toBlizzard :: Cell -> Maybe Dir
toBlizzard (Blizzard d) = Just d
toBlizzard _ = Nothing

toCell :: Int -> Int -> Char -> (Point, Cell)
toCell y x '#' = (newPoint x y, Wall)
toCell y x '.' = (newPoint x y, Empty)
toCell y x c = (newPoint x y, Blizzard $ charToDir c)

simulatePoint :: Map Point Point -> (Point, [Dir]) -> [(Point, [Dir])]
simulatePoint walls (point, dirs) = map (\d -> (f d, [d])) dirs
  where f d = M.findWithDefault res res walls -- If we're in a wall, teleport to the other side
          where res = point + dirToDelta d

simulateBlizzards :: Map Point Point -> Map Point [Dir] -> Int -> [Map Point [Dir]]
simulateBlizzards walls current 0 = [current]
simulateBlizzards walls current n = current : simulateBlizzards walls current' (n - 1)
  where current' = foldl (\m (p,ds) -> M.insertWith (++) p ds m) M.empty $ concatMap (simulatePoint walls) $ M.toList current

neighboursAndSelf = newPoint 0 0 : neighbours

getNeighbours :: Set Point -> Map Point [Dir] -> Point -> [Point]
getNeighbours walls blizzards pos = filter (`M.notMember` blizzards) $ filter (`S.notMember` walls) $ map (pos +) neighboursAndSelf

addTime :: Int -> Point -> (Int, Point)
addTime time a = (time, a)

getEdges :: Set Point -> [Point] -> [Map Point [Dir]] -> Int -> Int -> [[((Int, Point), (Int, Point))]]
getEdges walls points [] time cycleAt = []
getEdges walls points (blizzards:rest) time cycleAt = edges : getEdges walls points'' rest (time + 1) cycleAt
  where edges = concatMap (\p -> map (((time `rem` cycleAt, p),) . addTime ((time + 1) `rem` cycleAt)) $ getNeighbours walls blizzards p) points
        points' = trace (printf "Time is at %d\n" time) $ S.toList $ S.fromList $ map (snd . snd) edges
        points'' = trace (printf "Number of points to consider is %d" (length points')) points'

gridToString :: [Int] -> [Int] -> (Int -> Int -> Char) -> [String]
gridToString x y showFunc = map (\y -> map (showFunc y) x) y

getOpposite :: Int -> Int -> Int -> Int -> Point -> Point
getOpposite minX maxX minY maxY point
  | getX point == minX = newPoint (maxX - 1) (getY point)
  | getX point == maxX = newPoint (minX + 1) (getY point)
  | getY point == minY = newPoint (getX point) (maxY - 1)
  | getY point == maxY = newPoint (getX point) (minY + 1)
  | otherwise = point

solve = do
  chs <- lines <$> getContents
  let grid = concat $ zipWith (\ y row -> zipWith (toCell y) [0 .. ] row) [0..] chs
  let walls = S.fromList $ map fst $ filter ((== Wall) . snd) grid
  let blizzards = M.fromList $ mapMaybe (\(p, c) -> (p, ) . (: []) <$> toBlizzard c) grid

  let maxX = maximum (map (getX . fst) grid)
  let maxY = maximum (map (getY . fst) grid)

  let wallsWithOpposite = M.fromList $ map (\w -> (w, getOpposite 0 maxX 0 maxY w)) $ S.toList walls
  let startingPlane = map fst $ filter ((== Empty) . snd) grid

  let cycleAt = lcm (maxX - 1) (maxY - 1) -- We know there's at most this many blizzard generations
  let blizzards' = simulateBlizzards wallsWithOpposite blizzards (cycleAt - 1)
  let blizzards'' = reverse $ head blizzards' : reverse (tail blizzards')

  let startPoint = fst $ head $ filter ((/= Wall) . snd) $ filter ((== 0) . getY . fst) grid
  let start = (0, startPoint)
  let end = fst $ head $ filter ((/= Wall) . snd) $ filter ((== maxY) . getY . fst) grid

  let walls' = S.insert (end + newPoint 0 1) $ S.insert (startPoint + newPoint 0 (-1)) walls

  let edges = getEdges walls' startingPlane blizzards'' 0 cycleAt
  let edges' = foldl (foldl (\m (s, e) -> M.insertWith (++) s [e] m)) M.empty edges

  let pathFirst = reverse $ Data.Maybe.fromMaybe [] $ bfsPathCond edges' start (\a -> snd a == end)
  let showFunc ((_, p), b) y x =
        if newPoint x y == p then 'E'
        else
          case M.findWithDefault [] (newPoint x y) b of
            [] -> '.'
            [d] -> dirToChar d
            l -> head $ show $ length l
  --printf "%s" (intercalate "\n\n" $ map (intercalate "\n") $ map (\b -> gridToString [0..maxX] [0..maxY] (showFunc b)) (zip path (cycle blizzards')))
  let start = last pathFirst
  let pathBack = reverse $ Data.Maybe.fromMaybe [] $ bfsPathCond edges' start (\a -> snd a == startPoint)
  let start = last pathBack
  let pathAgain = reverse $ Data.Maybe.fromMaybe [] $ bfsPathCond edges' start (\a -> snd a == end)

  --printf "\n\nPath was %s" (show path)

  printf "Found a path which takes %d minutes\n" (sum [length pathFirst - 1, length pathBack - 1, length pathAgain - 1])
