module Day18.Part1 (solve) where
import Text.Printf (printf)
import Lib (Point, newPoint3, neighbours3)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, endOfLine, endBy1)
import Data.Set (fromList, Set, member, elems)
import qualified Data.Set as S

parsePoint :: Parser Point
parsePoint = newPoint3 <$> (number <* char ',') <*> (number <* char ',') <*> number

parse :: Parser [Point]
parse = parsePoint `endBy1` endOfLine

countVisibleSides :: Set Point -> Point -> Int
countVisibleSides points point = length n - length (filter (`member` points) n)
  where n = map (point +) neighbours3


solve = do
  pointSet <- fromList <$> parseInput parse
  printf "I can see %d sides\n" (sum $ map (countVisibleSides pointSet) (elems pointSet))
