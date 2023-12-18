module Day18.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, (<|>), many1, string, endOfLine, sepEndBy1)
import Data.Functor (($>))
import Text.Parsec.Char (noneOf)
import Helpers.Point (Point, newPoint, getX, getY)

data Dir = L | R | U | D

vec :: Int -> Dir -> Point
vec n L = newPoint (-n)  0
vec n R = newPoint   n   0
vec n U = newPoint   0   n
vec n D = newPoint   0 (-n)

pDir :: Parser Dir
pDir = (char 'L' $> L) <|> (char 'R' $> R) <|> (char 'U' $> U) <|> (char 'D' $> D)

pLine :: Parser (Dir, Int, String)
pLine = (,,) <$> (pDir <* char ' ') <*> (number <* string " (") <*> many1 (noneOf ")") <* char ')'

pInst :: Parser [(Dir, Int, String)]
pInst = pLine `sepEndBy1` endOfLine

poly :: Point -> [(Dir, Int, String)] -> [Point]
poly p [] = [p]
poly p ((d, n, _):is) = p : poly (p + vec n d) is

area :: [Point] -> Int
area polygon = abs $ sum (zipWith (\a b -> (getX b + getX a) * (getY b - getY a)) (last polygon : polygon) polygon) `div` 2

points :: [Point] -> Int
points path = area path + (length path `div` 2) - 1

len :: (Dir, Int, String) -> Int
len (_, b, _) = b

solve :: IO ()
solve = do
  inst <- parseInput pInst
  let start = newPoint 0 0
  let polygon' = reverse (poly start inst)
  printf "Last is %s\n" (show $ head polygon')
  printf "Outline is %d\n" (sum (map len inst) `div` 2)
  printf "The area is %d\n" (area (start : polygon'))
