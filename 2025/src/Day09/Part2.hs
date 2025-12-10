module Day09.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (point2D, Parser, parseInput)
import Text.Parsec (endOfLine, endBy1)
import Helpers.Point
import Helpers.List (pairsOther)

-- First in is the primary axis, other two the min and max point of the line
data Line = Horizontal Int Int Int | Vertical Int Int Int

newLine :: Point -> Point -> Line
newLine a b
  | minX == maxX = Vertical minX minY maxY
  | otherwise = Horizontal minY minX maxX
  where minX = min (getX a) (getX b)
        maxX = max (getX a) (getX b)
        minY = min (getY a) (getY b)
        maxY = max (getY a) (getY b)

inputP :: Parser [Point]
inputP = point2D `endBy1` endOfLine

crossingNumberVertical :: Point -> [Line] -> Int
crossingNumberVertical p ls = length $ filter crossing ls
  where crossing (Horizontal {}) = False
        crossing (Vertical x' minY maxY) = getX p < x' && minY <= getY p && getY p <= maxY

crossingNumberHorizontal :: Point -> [Line] -> Int
crossingNumberHorizontal p ls = length $ filter crossing ls
  where crossing (Horizontal {}) = False
        crossing (Vertical x' minY maxY) = getX p < x' && minY <= getY p && getY p <= maxY


polygonContains :: [Line] -> Point -> Bool
polygonContains ls p = crossingNumber'
  where crossingNumber' = odd (crossingNumberVertical p ls) || odd (crossingNumberHorizontal p ls)

rectangleSize :: Point -> Point -> Int
rectangleSize a b = dx * dy
  where dx = 1 + abs (getX a - getX b)
        dy = 1 + abs (getY a - getY b)

lineSegments :: [Point] -> [Line]
lineSegments points = zipWith newLine points (drop 1 points ++ take 1 points)

rectangleCorners :: Point -> Point -> [Point]
rectangleCorners a b = [newPoint minX minY, newPoint minX maxY, newPoint maxX minY, newPoint maxX maxY]
  where minX = min (getX a) (getX b) + 1
        maxX = max (getX a) (getX b) - 1
        minY = min (getY a) (getY b) + 1
        maxY = max (getY a) (getY b) - 1

intersects :: Line -> Line -> Bool
intersects (Horizontal y' minX maxX) (Vertical x' minY maxY) = minX <= x' && x' <= maxX && minY <= y' && y' <= maxY
intersects (Vertical x' minY maxY) (Horizontal y' minX maxX) = minX <= x' && x' <= maxX && minY <= y' && y' <= maxY
intersects _ _ = False

checkRectangle :: [Line] -> (Point, Point) -> Bool
checkRectangle ls (a, b) = all (polygonContains ls) corners && not rectLineCrosses
  where corners = rectangleCorners a b
        rectLines = zipWith newLine corners (last corners : corners)
        rectLineCrosses = any (\rl -> any (intersects rl) ls) rectLines

solve :: IO ()
solve = do
  points <- parseInput inputP
  let ls = lineSegments points
  let rectangleSizes = map (uncurry rectangleSize) $ filter (checkRectangle ls) $ pairsOther points
  printf "The biggest rectangle size within the tiles is %d\n" (maximum rectangleSizes)
