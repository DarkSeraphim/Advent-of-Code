{-# LANGUAGE TupleSections #-}
module Day21.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGridMaybe)
import Helpers.Point (Point, neighbours, getY, getX, newPoint)
import Data.Set (Set)
import Data.Map (Map, findWithDefault, keys, size)
import qualified Data.Set as S
import qualified Data.Map as M

data Bounds = Bounds {minX :: Int, minY :: Int, maxX :: Int, maxY :: Int} deriving Show

mkBounds :: Map Point a -> Bounds
mkBounds grid = Bounds {minX = minX', maxX = maxX', minY = minY', maxY = maxY'}
  where xs = map getX $ keys grid
        ys = map getY $ keys grid
        minX' = minimum xs
        maxX' = maximum xs
        minY' = minimum ys
        maxY' = maximum ys

incMax :: Bounds -> Point -> Bounds
incMax bounds point = bounds {maxX = maxX bounds + getX point, maxY = maxY bounds + getY point}

data Plot = Elf | Garden | Rocks deriving (Eq, Ord)
type FindPlot = Map Point Plot -> Point -> Plot

fromChar :: Char -> Maybe Plot
fromChar '#' = Just Rocks
fromChar '.' = Just Garden
fromChar 'S' = Just Elf
fromChar  _  = Nothing

findPlot :: Bounds -> FindPlot
findPlot _ grid point = findWithDefault Rocks point grid

findNext :: FindPlot -> Map Point Plot -> Point -> [Point]
findNext fp grid start = fm neighbours
  where f = filter (\k -> Garden == fp grid k)
        m = map (start +)
        fm = f . m
flood :: FindPlot -> Map Point Plot -> Map Point Int -> Set Point -> Int -> Int -> Map Point Int
flood fp grid dist starts end n
  | end < n = dist
  | otherwise = flood fp grid dist' starts' end (n + 1)
  where starts' = S.fromList $ filter (`M.notMember` dist) $ concatMap (findNext fp grid) (S.toList starts)
        dist' = M.union dist (M.fromList $ map (, n) (S.toList starts'))

solve :: IO ()
solve = do
  grid <- M.mapKeys (flip (-) (newPoint 1 1)) <$> parseGridMaybe fromChar
  let elf =  (fst . head . filter ((==Elf) . snd) . M.toList) grid
  let bounds = ((`incMax` newPoint 1 1) . mkBounds) grid
  printf "Bounds are %s\n" (show bounds)
  let grid' = M.insert elf Garden grid
  let fl steps = flood (findPlot bounds) grid' (M.singleton elf 0) (S.singleton elf) steps 1
  let dists = fl 131
  let evenCorners = size $ M.filter (\v -> even v && v > 65) dists
  let evenFull = size $ M.filter even dists
  let oddCorners = size $ M.filter (\v -> odd v && v > 65) dists
  let oddFull = size $ M.filter odd dists


  let target = (26501365 - 65) `div` 131;
  -- Thanks to https://github.com/villuna/aoc23/wiki/A-Geometric-solution-to-advent-of-code-2023,-day-21 for the help on figuring out how I could actually just count entire blocks
  let result = ((target+1)*(target+1)) * oddFull + (target*target) * evenFull - (target+1) * oddCorners + target * evenCorners;

  printf "We can visit %d garden plots\n" result
