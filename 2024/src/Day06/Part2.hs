module Day06.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Input (toPointMap)
import Data.Map (partition, keys, Map)
import qualified Data.Map as M
import Helpers.Point (Point, getY, getX, newPoint)
import Data.Set (Set, insert, empty, size, member, toList)

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
      Just Empty -> walk grid (pos + dir) dir visited'
      Just Guard -> error "Guard should've been removed"
      Just Object -> walk grid pos rotDir visited
      Nothing -> visited'
  where nextTile = M.lookup (pos + dir) grid
        visited' = insert pos visited
        rotDir = rotate dir

findCycle' :: Map Point Tile -> Point -> Dir -> Set (Point, Dir) -> Bool
findCycle' grid pos dir seenCollisions =
  case nextTile of
    Just Empty -> findCycle' grid (pos + dir) dir seenCollisions
    Just Guard -> error "Guard should've been removed"
    Just Object ->
      (collision `member` seenCollisions) || findCycle' grid pos rotDir collided
    Nothing -> False
  where
    nextTile = M.lookup (pos + dir) grid
    rotDir = rotate dir
    collision = (pos, dir)
    collided = insert collision seenCollisions

findCycle :: Map Point Tile -> Point -> Dir -> Point -> Bool
findCycle grid pos dir newObjectPos 
  | newObjectPos == pos = False
  | otherwise = findCycle' grid' pos dir empty
  where grid' = M.insert newObjectPos Object grid

solve :: IO ()
solve = do
  grid' <- toPointMap toTile . (reverse . lines) <$> getContents
  let (guard, grid) = partition (== Guard) grid'
      guardPos = head (keys guard)
      grid'' = M.insert guardPos Empty grid
  let visitedSet = walk grid'' guardPos (newPoint 0 1) empty

  let possible = filter (findCycle grid'' guardPos (newPoint 0 1)) $ toList visitedSet


  printf "We can place objects on %d locations for a loop\n" (length possible)

