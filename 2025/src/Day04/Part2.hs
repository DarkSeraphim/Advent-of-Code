module Day04.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGridMaybe)
import Helpers.Point (Point, neighboursDiag)
import Data.Map (Map, findWithDefault, keys, delete, size)
import qualified Data.Map (filter)

data Cell = Roll | Empty deriving Eq
type Grid = Map Point Cell

toCell :: Char -> Maybe Cell
toCell '@' = Just Roll
toCell  _  = Nothing

countAdjecent :: Grid -> Point -> Int
countAdjecent grid point = length adj
  where adj = filter (Roll ==) $ map ((\x -> findWithDefault Empty x grid) . (point +)) neighboursDiag

removeRolls :: Grid -> (Grid, Bool)
removeRolls grid = (foldl (flip delete) grid removable, not (null removable))
  where rolls = keys $ Data.Map.filter (Roll ==) grid
        removable = filter ((< 4 ) . countAdjecent grid) rolls

removeRollsRecursively :: Grid -> Grid
removeRollsRecursively grid = if removed then removeRollsRecursively grid' else grid
  where (grid', removed) = removeRolls grid

solve :: IO ()
solve = do
  grid <- parseGridMaybe toCell
  let res = removeRollsRecursively grid
  printf "We can remove %d rolls\n" (size grid - size res)
