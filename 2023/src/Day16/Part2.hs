{-# LANGUAGE TupleSections #-}
module Day16.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec(parseGridMaybe)
import Helpers.Point (Point, newPoint, getY, getX)
import Data.Set (Set, insert, empty)
import qualified Data.Set as S
import Data.Map (Map, (!), keys)
import qualified Data.Map as M

data Reflector = Hor | Ver | For | Back | Empty deriving (Eq, Show)

fromChar :: Char -> Maybe Reflector
fromChar '|' = Just Ver
fromChar '-' = Just Hor
fromChar '/' = Just For
fromChar '\\' = Just Back
fromChar  _  = Just Empty

reflect :: Point -> Reflector -> [Point]
reflect p Empty = [p]
reflect p Hor
  | getY p == 0 = [p]
  | otherwise = [newPoint 1 0, newPoint (-1) 0]
reflect p Ver
  | getX p == 0 = [p]
  | otherwise = [newPoint 0 1, newPoint 0 (-1)]
reflect p Back = [newPoint (getY p) (getX p)]
reflect p For = [newPoint (- (getY p)) (- (getX p))]

walk :: Point -> Point -> Map Point Reflector -> Set (Point, Point) -> Set (Point, Point)
walk pos dir grid visited
  -- Don't record outside points, just ignore them
  | pos `M.notMember` grid = visited
  -- We don't need to visit these twice
  | (pos, dir) `S.member` visited = visited
  | otherwise = foldl (\s nd -> walk (pos + nd) nd grid s) visited' nextDir
  where nextDir = reflect dir (grid ! pos)
        visited' = insert (pos, dir) visited

solve :: IO ()
solve = do
  grid <- parseGridMaybe fromChar
  let xs = map getX $ keys grid
  let ys = map getY $ keys grid
  let minX = minimum xs
  let maxX = maximum xs
  let minY = minimum ys
  let maxY = maximum ys


  let starting = concat [
                   map ((, newPoint   0   1 ) . (`newPoint` minY)) [minX..maxX],
                   map ((, newPoint   0 (-1)) . (`newPoint` maxY)) [minX..maxX],
                   map ((, newPoint   1   0 ) . newPoint minX) [minY..maxY],
                   map ((, newPoint (-1)  0 ) . newPoint maxX) [minY..maxY]
                 ]

  let visited = foldl (\best (start, dir) -> max best $ length $ S.map fst $ walk start dir grid empty) minBound starting
  printf "# of energized tiles is %d\n" visited
