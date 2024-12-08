module Day08.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Input (toPointMap)
import Data.Map (Map, toList, fromListWith, elems)
import qualified Data.Map as M
import qualified Data.Set as S
import Helpers.Point (Point)

flipMap :: Ord a => Map k a -> Map a [k]
flipMap m = fromListWith (++) $ map (\(k, a) -> (a, [k])) (toList m)

getAntinodes :: [Point] -> [Point]
getAntinodes points = [x - (y - x) | x <- points, y <- points, x /= y]

solve :: IO ()
solve = do
  antennas <- toPointMap id . lines <$> getContents
  let byAntenna = flipMap $ M.filter (/= '.') antennas

  let antinodes = map getAntinodes (elems byAntenna)
  let result = filter (`M.member` antennas) $ concat antinodes
  printf "Amount of antinodes: %d\n" (S.size $ S.fromList result)
