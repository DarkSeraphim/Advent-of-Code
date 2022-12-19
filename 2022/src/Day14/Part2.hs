{-# LANGUAGE TupleSections #-}
module Day14.Part2 (solve) where
import Text.Printf (printf)
import Lib (Point, newPoint, y, x)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, sepBy1, string, endOfLine, endBy1)
import Data.Map (fromList, member, keys, findWithDefault, Map, notMember, insert)
import qualified Data.Map as M
import Data.List (intercalate)

parsePoint :: Parser Point
parsePoint = newPoint <$> (number <* char ',') <*> number

parseLine :: Parser [Point]
parseLine = parsePoint `sepBy1` string " -> "

parse :: Parser [[Point]]
parse = parseLine `endBy1` endOfLine

getLines :: (Point, Point) -> [Point]
getLines (from, to)
  | from == to = [from]
  | otherwise  = from : getLines (from', to)
  where from' = from + signum (to - from)

down = newPoint 0 1
left = newPoint (-1) 1
right = newPoint 1 1

simulateSand :: Point -> Map Point a -> Int -> Point
simulateSand start grid lowest
  | y down' == lowest = start
  | down' `notMember` grid = simulateSand down' grid lowest
  | left' `notMember` grid = simulateSand left' grid lowest
  | right' `notMember` grid = simulateSand right' grid lowest
  | otherwise = start
  where down' = start + down
        left' = start + left
        right' = start + right

run :: Point -> Map Point Char -> Int -> Map Point Char
run start grid lowest =
  if next == start then putSand next else run start (putSand next) lowest
  where next = simulateSand start grid lowest
        putSand pos = insert pos 'o' grid

solve = do
  lines <- parseInput parse
  let points = fromList $ map (,'#') $ concatMap (\lines' -> concat $ zipWith (curry getLines) lines' (tail lines')) lines
  let start = newPoint 500 0

  let minX = minimum $ map x $ keys points
  let maxX = maximum $ map x $ keys points
  let minY = 0 --minimum $ map y $ keys points
  let maxY = maximum $ map y $ keys points

  let c = map (\y -> map (\x -> findWithDefault '.' (newPoint x y) points) [minX..maxX]) [minY..maxY]

  printf "Grid:\n%s\n" (intercalate "\n" c)

  let result = run start points (maxY + 2)
  let c = map (\y -> map (\x -> findWithDefault '.' (newPoint x y) result) [minX..maxX]) [minY..maxY]

  printf "Grid:\n%s\n" (intercalate "\n" c)
  printf "%d units of sand rest on the cave\n" (length $ filter (=='o') $ M.elems result)
