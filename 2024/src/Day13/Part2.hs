module Day13.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseInput, numberInteger)
import Helpers.Point (PointBig, newPoint, getY, getX)
import Text.Parsec (string, anyChar, endOfLine, sepBy1)
import Data.Maybe (mapMaybe)

button :: Parser PointBig
button = newPoint <$> (string "Button " *> anyChar *> string ": X" *> numberInteger)
          <*>
            (string ", Y" *> numberInteger) <* endOfLine

prize :: Parser PointBig
prize = (prizeOffset +) <$> (newPoint <$> (string "Prize: X=" *> numberInteger) <*> (string ", Y=" *> numberInteger) <* endOfLine)

prizeOffset :: PointBig
prizeOffset = newPoint 10000000000000 10000000000000

scenario :: Parser (PointBig, PointBig, PointBig)
scenario = (,,) <$> button <*> button <*> prize

scenarios :: Parser [(PointBig, PointBig, PointBig)]
scenarios = scenario `sepBy1` endOfLine

solveSystem :: (PointBig, PointBig, PointBig) -> Maybe (Integer, Integer)
solveSystem (a, b, p)
  | scale n * a + scale m * b == p = Just (n, m)
  | otherwise = Nothing
  where ax = fromIntegral $ getX a :: Double
        ay = fromIntegral $ getY a :: Double
        bx = fromIntegral $ getX b :: Double
        by = fromIntegral $ getY b :: Double
        px = fromIntegral $ getX p :: Double
        py = fromIntegral $ getY p :: Double
        n = round $ (px - (bx * py) / by) / (ax - (bx * ay) / by) :: Integer
        m = round $ (py - ay * fromIntegral n) / by :: Integer
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


computeCost :: (Integer, Integer) -> Integer
computeCost (a, b) = 3 * a + b

solve :: IO ()
solve = do
  scenarios' <- parseInput scenarios
  let solved = map solveSystem scenarios'
  let result = sum $ mapMaybe (fmap computeCost) solved
  printf "Scenarios: %d" result
