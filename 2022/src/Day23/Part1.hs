{-# LANGUAGE TupleSections #-}
module Day23.Part1 (solve) where
import Text.Printf (printf)
import Lib (Point, newPoint, getX, getY)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Set (fromList, Set, notMember, toList, member)
import Data.List (find, intercalate)
import Helpers.List (frequency)
import Data.Map ((!))

toPoint :: Int -> Int -> Char -> Maybe Point
toPoint y x '#' = Just $ newPoint x y
toPoint y x _ = Nothing

steps :: [[Point]]
steps = cycle $ map (map (uncurry newPoint)) [
    [( 0, -1), ( 1, -1), (-1, -1)], -- N, NE, NW
    [( 0,  1), ( 1,  1), (-1,  1)], -- S, SE, SW
    [(-1,  0), (-1, -1), (-1,  1)], -- W, NW, SW
    [( 1,  0), ( 1, -1), ( 1,  1)]  -- E, NE, SE
  ]

allNeighbours = map (uncurry newPoint) [
    ( 0, -1), -- N
    ( 1, -1), -- NE
    ( 1,  0), -- E
    ( 1,  1), -- SE
    ( 0,  1), -- S
    (-1,  1), -- SW
    (-1,  0), -- W
    (-1, -1)  -- NW
  ]
-- drop 1, take 4

self :: Point
self = newPoint 0 0

isValidStep :: Set Point -> Point -> [Point] -> Bool
isValidStep elves elf = all ((`notMember` elves) . (elf +)) . filter (/= self)

takeSteps :: Set Point -> [[Point]] -> Int -> Set Point
takeSteps elves steps 0 = elves
takeSteps elves steps moves = takeSteps elves' (drop 1 steps) (moves - 1)
  where stepsToConsider = (self : allNeighbours) : take 4 steps
        -- Find valid moves, or default with the same location
        beforeAfter = map (\elf -> (elf,) . maybe elf ((elf +) . head) $ find (isValidStep elves elf) stepsToConsider) (toList elves)
        freq = frequency (map snd beforeAfter)
        elves' = fromList (map (\(b, a) -> if freq ! a == 1 then a else b) beforeAfter)

solve = do
  chs <- lines <$> getContents
  let points = fromList $ catMaybes $ concat $ zipWith (\y row -> zipWith (toPoint y) [0..] row) [0..] chs
  let elves = toList $ takeSteps points steps 10
  let minX = minimum (map getX elves)
  let maxX = maximum (map getX elves)
  let minY = minimum (map getY elves)
  let maxY = maximum (map getY elves)

  let elves' = fromList elves
  let g = map (\y -> map (\x -> if newPoint x y `member` elves' then '#' else '.') [minX..maxX]) [minY..maxY]

  printf "%d - %d, %d - %d" minX maxX minY maxY
  printf "Grid:\n%s\n" (intercalate "\n" (g))

  printf "Empty spaces: %d" ((maxY - minY + 1) * (maxX - minX + 1) - length elves)
