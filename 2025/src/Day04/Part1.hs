module Day04.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Helpers.Point (Point, neighboursDiag)
import Data.Map (Map, findWithDefault, keys)
import qualified Data.Map (filter)

data Cell = Roll | Empty deriving Eq
type Grid = Map Point Cell

toCell :: Char -> Cell
toCell '@' = Roll
toCell  _  = Empty

countAdjecent :: Grid -> Point -> Int
countAdjecent grid point = length adj
  where adj = filter (Roll ==) $ map ((\x -> findWithDefault Empty x grid) . (point +)) neighboursDiag

solve :: IO ()
solve = do
  grid <- parseGrid toCell
  let rolls = keys $ Data.Map.filter (Roll ==) grid
  let res = length $ filter (< 4) $ map (countAdjecent grid) rolls
  printf "We can access %d rolls\n" res
