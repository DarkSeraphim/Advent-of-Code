module Day18.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Helpers.Point (Point, neighboursDiag)
import Data.Map (Map, findWithDefault)
import qualified Data.Map as Map

isOn :: Map Point Char -> Point -> Char
isOn grid point = if (self && around == 2) || around == 3 then '#' else '.'
  where on p = findWithDefault '.' p grid == '#'
        self = on point
        around = length $ filter (on . (point +)) neighboursDiag

animate :: Map Point Char -> Map Point Char
animate grid = Map.mapWithKey (\k _ -> isOn grid k) grid

applyN :: Int -> (a -> a) -> a -> a
applyN n f a = iterate f a !! n

solve :: IO ()
solve = do
  lights <- parseGrid id
  let res = applyN 100 animate lights
  printf "After the simulation, we have %d lights which are on.\n" (length $ Map.filter (=='#') res)
