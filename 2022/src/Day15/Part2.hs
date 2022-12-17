module Day15.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (string, endBy1, endOfLine)
import Helpers.Parsec (number, Parser, parseInput)
import Lib (Point, getX, getY, newPoint, manhattan, Range, newRange, compress, includes)
import Data.Set (fromList, elems)
import Debug.Trace (trace)
import Data.List (sortOn, sort)
import Data.Maybe (catMaybes, mapMaybe)

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

maxCoord = 4000000

isInRange :: Point -> (Point, Int) -> Bool
isInRange point (scanner, range) = manhattan point scanner <= range

toFrequency :: Point -> Integer
toFrequency p = toInteger (getX p) * 4000000 + toInteger (getY p)

toImpossibleRange :: Int -> (Point, Int) -> Maybe (Range Int)
toImpossibleRange x (s, r)
  | yR >= 0 = Just $ newRange (y - yR) (y + yR)
  | otherwise = Nothing
  where yR = r - abs (getX s - x)
        y = getY s

toImpossibilities :: [(Point, Int)] -> Int -> [Range Int]
toImpossibilities swr x = compress ranges
  where ranges = sort $ mapMaybe (toImpossibleRange x) swr


solve = do
  sensorPlusBeacon <- parseInput parse
  let sensorPlusRadius = sortOn (negate . snd) $ map sensorWithRadius sensorPlusBeacon
  let uniqueBeacons = elems (fromList (map snd sensorPlusBeacon))
  let range = [0..maxCoord]
  let possible = head $ filter (\(i, rs) -> length rs > 1) $ zip range $ map (toImpossibilities sensorPlusRadius) range
  let x = fst possible
  let y = head $ filter (\y -> not $ any (includes y) (snd possible)) range
  printf "We found %s locations\n" (show $ toFrequency $ newPoint x y)
  printf "Hello\n"
