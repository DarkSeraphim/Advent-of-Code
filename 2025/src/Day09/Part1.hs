module Day09.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (point2D, Parser, parseInput)
import Text.Parsec (endOfLine, endBy1)
import Helpers.Point
import Helpers.List (pairsOther)

inputP :: Parser [Point]
inputP = point2D `endBy1` endOfLine

rectangleSize :: Point -> Point -> Int
rectangleSize a b = dx * dy
  where dx = 1 + abs (getX a - getX b)
        dy = 1 + abs (getY a - getY b)

solve :: IO ()
solve = do
  points <- parseInput inputP
  let rectangleSizes = map (uncurry rectangleSize) $ pairsOther points
  printf "The biggest rectangle size is %d\n" (maximum rectangleSizes) 
