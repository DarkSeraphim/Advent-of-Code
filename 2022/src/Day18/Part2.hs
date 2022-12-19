{-# LANGUAGE TupleSections #-}
module Day18.Part2 (solve) where
import Text.Printf (printf)
import Lib (Point, newPoint3, neighbours3, getX, getY, getZ)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, endOfLine, endBy1)
import Data.Set (fromList, Set, member, elems, notMember, insert, singleton, union)
import qualified Data.Set as S
import Debug.Trace (trace)

parsePoint :: Parser Point
parsePoint = newPoint3 <$> (number <* char ',') <*> (number <* char ',') <*> number

parse :: Parser [Point]
parse = parsePoint `endBy1` endOfLine

countVisibleSides :: Set Point -> Point -> Int
countVisibleSides points point = length (filter (`member` points) n)
  where n = map (point +) neighbours3

floodFill :: Set Point -> Set Point -> Point -> Set Point
floodFill edges visited current = foldl (floodFill edges) visited' next''
  where next = map (current +) neighbours3
        next' = filter (`notMember` edges) next
        next'' = filter (`notMember` visited) next'
        visited' = foldl (flip insert) visited next''

getCubeXFace :: Int -> [(Int, Int)] -> [Point]
getCubeXFace x = map (\(a, b) -> newPoint3 x a b)

getCubeYFace :: Int -> [(Int, Int)] -> [Point]
getCubeYFace y = map (\(a, b) -> newPoint3 a y b)

getCubeZFace :: Int -> [(Int, Int)] -> [Point]
getCubeZFace z = map (\(a, b) -> newPoint3 a b z)

solve = do
  points <- parseInput parse
  let minX = (-2) + minimum (map getX points)
  let minY = (-2) + minimum (map getY points)
  let minZ = (-2) + minimum (map getZ points)
  let maxX = 2 + maximum (map getX points)
  let maxY = 2 + maximum (map getY points)
  let maxZ = 2 + maximum (map getZ points)

  let maxD = maximum [maxX, maxY, maxZ]
  let minD = minimum [minX, minY, minZ]
  -- Build edges and floodfill
  let planePoints = concatMap (\a -> map (a,) [minD..maxD]) [minD..maxD]
  let faces = fromList $ concat [getCubeXFace minD planePoints, getCubeXFace maxD planePoints, getCubeYFace minD planePoints, getCubeYFace maxD planePoints, getCubeZFace minD planePoints, getCubeZFace maxD planePoints]
  let start = newPoint3 (minX + 1) (minY + 1) (minZ + 1)
  let outside = floodFill (faces `union` fromList points) (singleton start) start 

  printf "I can see %d sides\n" (sum $ map (countVisibleSides outside) points)
