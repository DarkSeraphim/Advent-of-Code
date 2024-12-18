module Day18.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Point (Point, newPoint, neighbours, getY, getX)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, endOfLine, endBy1)
import Data.Set (fromList, Set, notMember, member)
import Helpers.Graph (bfsPathDyn)
import Helpers.Output (showGrid)
import qualified Data.Map

point :: Parser Point
point = newPoint <$> number <* char ',' <*> number

points :: Parser [Point]
points = point `endBy1` endOfLine

boundsCheck :: Point -> Point -> Point -> Bool
boundsCheck mi ma p = minX <= px && px <= maxX && minY <= py && py <= maxY
  where minX = getX mi
        minY = getY mi
        maxX = getX ma
        maxY = getY ma
        px = getX p
        py = getY p

efunc :: Set Point -> Point -> Point -> Point -> [Point]
efunc walls mi ma p = filter (boundsCheck mi ma) $ filter (`notMember` walls) $ map (p +) neighbours

solve :: IO ()
solve = do
  ps <- parseInput points
  let kb = take 1024 ps
      walls = fromList kb

  let start = newPoint 0 0
  let end = newPoint 70 70

  let grid = Data.Map.fromList [ (newPoint x y, if newPoint x y `member` walls then '#' else '.') | x <- [0..6], y <- [0..6]]

  printf "\n\n%s\n\n" (showGrid  grid id 'X')

  let maybePath = bfsPathDyn (efunc walls start end) start end

  printf "Min path length after 1kB is %d\n" (maybe (-1) length maybePath - 1)
