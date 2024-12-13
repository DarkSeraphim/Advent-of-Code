module Day13.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Helpers.Point (Point, newPoint, getY, getX)
import Text.Parsec (string, anyChar, endOfLine, sepBy1)
import Data.Maybe (mapMaybe)

button :: Parser Point
button = newPoint <$> (string "Button " *> anyChar *> string ": X" *> number)
          <*>
            (string ", Y" *> number) <* endOfLine

prize :: Parser Point
prize = newPoint <$> (string "Prize: X=" *> number) <*> (string ", Y=" *> number) <* endOfLine

scenario :: Parser (Point, Point, Point)
scenario = (,,) <$> button <*> button <*> prize

scenarios :: Parser [(Point, Point, Point)]
scenarios = scenario `sepBy1` endOfLine

solveSystem :: (Point, Point, Point) -> Maybe (Int, Int)
solveSystem (a, b, p)
  | scale n * a + scale m * b == p = Just (n, m)
  | otherwise = Nothing
  where ax = fromIntegral $ getX a :: Float
        ay = fromIntegral $ getY a :: Float
        bx = fromIntegral $ getX b :: Float
        by = fromIntegral $ getY b :: Float
        px = fromIntegral $ getX p :: Float
        py = fromIntegral $ getY p :: Float
        n = round $ (px - (bx * py) / by) / (ax - (bx * ay) / by) :: Int
        m = round $ (py - ay * fromIntegral n) / by :: Int
        scale s = newPoint s s

-- px = ax * A + bx * B
-- ax * A = px - bx * B
-- A = (px - bx * B) / ax
-- py = ay * A + by * B
-- by * B = py - ay * A
-- B = (py - ay * A) / by
-- A * ax = (px - bx * ((py - ay * A) / by))
-- A * ax = px - (bx * py) / by + (bx * ay * A) / by
-- A * ax - (bx * ay * A) / by = px - (bx * py) / by
-- A * ax - A * (bx * ay) / by
-- A * (ax - (bx * ay) / by) = px - (bx * py) / by
-- A = (px - (bx * py) / by) / ((ax - (bx * ay) / by))


computeCost :: (Int, Int) -> Int
computeCost (a, b) = 3 * a + b

solve :: IO ()
solve = do
  scenarios' <- parseInput scenarios
  let solved = map solveSystem scenarios'
  let result = sum $ mapMaybe (fmap computeCost) solved
  printf "Scenarios: %d" result
