module Day06.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Input (toPointMap)
import Data.Map (partition, keys, Map)
import qualified Data.Map as M
import Helpers.Point (Point, getY, getX, newPoint)
import Data.Set (Set, insert, empty, size)

data Tile = Object | Guard | Empty deriving Eq
type Dir = Point

toTile :: Char -> Tile
toTile '#' = Object
toTile '^' = Guard
toTile  _  = Empty

rotate :: Dir -> Dir
rotate d = newPoint (getY d) (- (getX d))

walk :: Map Point Tile -> Point -> Dir -> Set Point -> Set Point
walk grid pos dir visited = 
  case nextTile of
      Just Empty ->  walk grid (pos + dir) dir visited'
      Just Guard -> error "Guard should've been removed"
      Just Object -> walk grid pos rotDir visited
      Nothing -> visited'
  where nextTile = M.lookup (pos + dir) grid
        visited' = insert pos visited
        rotDir = rotate dir

solve :: IO ()
solve = do
  grid' <- toPointMap toTile . (reverse . lines) <$> getContents
  let (guard, grid) = partition (== Guard) grid'
      guardPos = head (keys guard)
  let visitedSet = walk (M.insert guardPos Empty grid) guardPos (newPoint 0 1) empty
  printf "We visited %d distinct points\n" (size visitedSet)
