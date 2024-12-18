module Day18.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Point (Point, newPoint, neighbours, getY, getX)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, endOfLine, endBy1)
import Data.Set (fromList, Set, notMember)
import Helpers.Graph (bfsPathDyn)

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

bruteforce :: [Point] -> Int -> Point
bruteforce ps i =
  case maybePath of
    Nothing -> last walls
    _       -> bruteforce ps (i + 1)
  where start = newPoint 0 0
        end = newPoint 70 70
        walls = take i ps
        maybePath = bfsPathDyn (efunc (fromList walls) start end) start end

toAnswer :: Point -> String
toAnswer p = printf "%d,%d" (getX p) (getY p)

solve :: IO ()
solve = do
  ps <- parseInput points
  let sol = bruteforce ps 1025
  printf "Path is impossible at byte: %s\n" (toAnswer sol)
