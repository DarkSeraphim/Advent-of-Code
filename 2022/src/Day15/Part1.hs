module Day15.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (string, endBy1, endOfLine)
import Helpers.Parsec (number, Parser, parseInput)
import Lib (Point (x, y), newPoint, manhattan)
import Data.Set (fromList, elems)

parsePoint :: Parser Point
parsePoint = newPoint <$> (string "x=" *> number) <*> (string ", y=" *> number)

parseLine :: Parser (Point, Point)
parseLine = (,) <$> (string "Sensor at " *> parsePoint) <*> (string ": closest beacon is at " *> parsePoint)

parse :: Parser [(Point, Point)]
parse = parseLine `endBy1` endOfLine

sensorWithRadius :: (Point, Point) -> (Point, Int)
sensorWithRadius (scanner, beacon) = (scanner, manhattan scanner beacon)

toList :: (a, a) -> [a]
toList (a, b) = [a, b]

isInRange :: Point -> (Point, Int) -> Bool
isInRange point (scanner, range) = manhattan point scanner <= range

solve = do
  sensorPlusBeacon <- parseInput parse
  let sensorPlusRadius = map sensorWithRadius sensorPlusBeacon
  let maxRadius = maximum $ map snd sensorPlusRadius
  let uniqueBeacons = elems (fromList (map snd sensorPlusBeacon))
  let xList = map x $ concatMap toList sensorPlusBeacon
  let minX = minimum xList - maxRadius
  let maxX = maximum xList + maxRadius
  let row = 2000000 :: Int
  let found = length $ filter (\x -> any (isInRange (newPoint x row)) sensorPlusRadius) [minX..maxX]
  let beacons = length $ filter ((==row) . y) uniqueBeacons
  printf "Manhattan: %d\n" (manhattan (newPoint 8 7) (newPoint 2 10))

  printf "At y = %d we found %d locations\n" row (found - beacons)
