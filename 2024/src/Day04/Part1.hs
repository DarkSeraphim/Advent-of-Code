module Day04.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Input (toPointMap)
import Data.Map (Map, findWithDefault, keys)
import qualified Data.Map as M
import Helpers.Point (Point, neighboursDiag)

isXmas :: Map Point Char -> Point -> Point -> Int
isXmas grid start dir
  | s == "XMAS" = 1
  | otherwise = 0
  where mPos = start + dir
        aPos = mPos + dir
        sPos = aPos + dir
        g p = findWithDefault '.' p grid
        s = [g start, g mPos, g aPos, g sPos]

getAll :: Map Point Char -> Point -> Int
getAll grid start = sum $ map (isXmas grid start) neighboursDiag

solve :: IO ()
solve = do
  grid <- toPointMap id . lines <$> getContents

  let starts = keys $ M.filter (== 'X') grid
      res = sum $ map (getAll grid) starts

  printf "Valid XMASes: %d\n" res
