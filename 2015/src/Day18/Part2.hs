module Day18.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Helpers.Point (Point, neighboursDiag, newPoint, getX, getY)
import Data.Map (Map, findWithDefault, findMin, findMax, insert)
import qualified Data.Map as Map

combo :: Point -> Point -> [Point]
combo a b = [a, newPoint bx ay, newPoint ax by, b]
  where ax = getX a
        ay = getY a
        bx = getX b
        by = getY b

turnCornersOn :: Map Point Char -> Map Point Char
turnCornersOn grid = foldl (\g p -> insert p '#' g) grid (combo minPoint maxPoint)
  where minPoint = fst $ findMin grid
        maxPoint = fst $ findMax grid

isOn :: Map Point Char -> Point -> Char
isOn grid point = if (self && around == 2) || around == 3 then '#' else '.'
  where on p = findWithDefault '.' p grid == '#'
        self = on point
        around = length $ filter (on . (point +)) neighboursDiag

animate :: Map Point Char -> Map Point Char
animate grid = turnCornersOn $ Map.mapWithKey (\k _ -> isOn grid k) grid

applyN :: Int -> (a -> a) -> a -> a
applyN n f a = iterate f a !! n

solve :: IO ()
solve = do
  lights <- turnCornersOn <$> parseGrid id
  let res = applyN 100 animate lights
  printf "After the simulation, we have %d lights which are on.\n" (length $ Map.filter (=='#') res)
