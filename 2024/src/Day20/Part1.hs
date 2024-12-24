module Day20.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Helpers.Point (Point, neighbours)
import Data.Map (Map, toList, keys, findWithDefault, member, fromList)
import Helpers.Graph (dijkstra''', rebuildPath)
import Data.Set (singleton, empty)
import Data.Maybe (fromJust, mapMaybe)

import qualified Data.Map as M

efunc :: Map Point Char -> Point -> [Point]
efunc grid p = n
  where n = filter (\p' -> findWithDefault '#' p' grid /= '#') $ map (p +) neighbours

findPoint :: Map Point Char -> Char -> Point
findPoint grid value = fst $ head $ filter ((value ==) . snd) $ toList grid

getPath :: Point -> Point -> Map Point Char -> [Point]
getPath start end grid = fromJust path' 
  where path = dijkstra''' (const 1) (efunc grid) empty (singleton (0, start, start)) (== end)
        path' = rebuildPath end path

isValidCheat :: Map Point Int -> Point -> Bool
isValidCheat idxMap point = length n' >= 2 
  where n = map (point +) neighbours
        n' = filter (`member` idxMap) n

getCheatSavings :: Map Point Int -> Point -> Int
getCheatSavings idxMap wall = maxIdx - minIdx - 2
  where n = map (wall +) neighbours
        n' = mapMaybe (`M.lookup` idxMap) n
        minIdx = minimum n'
        maxIdx = maximum n'


solve :: IO ()
solve = do
  grid <- parseGrid id
  let start = findPoint grid 'S'
      end = findPoint grid 'E'
      walls = keys $ M.filter (=='#') grid

  let path = reverse $ getPath start end grid
  let idxMap = fromList (zip path [0..])
  let cheats = filter (isValidCheat idxMap) walls
  let savings = filter (> 0) $ map (getCheatSavings idxMap) cheats

  printf "Savings: %d\n" (length $ filter ( >= 100) savings)
