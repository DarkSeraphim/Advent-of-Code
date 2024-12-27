module Day20.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Helpers.Point (Point, neighbours, manhattan)
import Data.Map (Map, toList, keys, findWithDefault, fromList, (!))
import Helpers.Graph (dijkstra''', rebuildPath)
import Data.Set (singleton, empty)
import Data.Maybe (fromJust)

efunc :: Map Point Char -> Point -> [Point]
efunc grid p = n
  where n = filter (\p' -> findWithDefault '#' p' grid /= '#') $ map (p +) neighbours

findPoint :: Map Point Char -> Char -> Point
findPoint grid value = fst $ head $ filter ((value ==) . snd) $ toList grid

getPath :: Point -> Point -> Map Point Char -> [Point]
getPath start end grid = fromJust path'
  where path = dijkstra''' (const 1) (efunc grid) empty (singleton (0, start, start)) (== end)
        path' = rebuildPath end path

getCheatSavings :: Map Point Int -> [Int]
getCheatSavings idxMap = map d validCombos
  where points = keys idxMap
        combos = [(a, b) | a <- points, b <- points, manhattan a b > 1]
        validCombos = filter ((<= 20) . uncurry manhattan) combos
        d (a, b)
          | ia < ib = ib - ia - manhattan a b
          | otherwise = 0
          where ia = i a
                ib = i b

        i p = idxMap ! p

solve :: IO ()
solve = do
  grid <- parseGrid id
  let start = findPoint grid 'S'
      end = findPoint grid 'E'

  let path = reverse $ getPath start end grid
  let idxMap = fromList (zip path [0..])
  let savings = filter (>= 50) $ getCheatSavings idxMap

  printf "Savings: %d\n" (length $ filter ( >= 100) savings)
