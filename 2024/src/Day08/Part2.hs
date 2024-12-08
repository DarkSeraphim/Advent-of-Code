module Day08.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Input (toPointMap)
import Data.Map (Map, toList, fromListWith, elems, keys)
import qualified Data.Map as M
import qualified Data.Set as S
import Helpers.Point (Point, getX, getY)

flipMap :: Ord a => Map k a -> Map a [k]
flipMap m = fromListWith (++) $ map (\(k, a) -> (a, [k])) (toList m)

untilBounds :: Int -> Int -> Point -> Point -> [Point]
untilBounds maxX maxY start delta
  | x < minX || x > maxX = []
  | y < minY || y > maxY = []
  | otherwise = start : untilBounds maxX maxY (start - delta) delta
  where minX = 0
        minY = 0
        x = getX start
        y = getY start

getAntinodes :: (Point -> Point -> [Point]) -> [Point] -> [Point]
getAntinodes f points = concat [f x (y - x) | x <- points, y <- points, x /= y]

solve :: IO ()
solve = do
  antennas <- toPointMap id . lines <$> getContents
  let byAntenna = flipMap $ M.filter (/= '.') antennas

  let points = keys antennas
  let maxX = maximum $ map getX points
  let maxY = maximum $ map getY points

  let antinodes = map (getAntinodes (untilBounds maxX maxY)) (elems byAntenna)
  let result = filter (`M.member` antennas) $ concat antinodes
  printf "Amount of antinodes: %d\n" (S.size $ S.fromList result)
