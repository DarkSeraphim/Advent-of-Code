module Day04.Part2 (solve) where
import Text.Printf (printf)
import Data.Map (Map, findWithDefault, keys)
import Helpers.Point (Point, newPoint)
import qualified Data.Map as M
import Helpers.Input (toPointMap)

validOpposites :: String -> Bool
validOpposites "MS" = True
validOpposites "SM" = True
validOpposites    _ = False

diagonals :: [[Point]]
diagonals = map (map (uncurry newPoint)) [[(-1, -1), (1, 1)], [(-1, 1), (1, -1)]]

getAll :: Map Point Char -> Point -> Bool
getAll grid start
  | all validOpposites d = True
  | otherwise = False
  where g p = findWithDefault '.' p grid
        d = map (map (g . (start +))) diagonals


solve :: IO ()
solve = do
  grid <- toPointMap id . lines <$> getContents

  let starts = keys $ M.filter (== 'A') grid
      res = length $ filter (getAll grid) starts

  printf "Valid XMASes: %d\n" res
