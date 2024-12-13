module Day12.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Input (toPointMap)
import Helpers.Point (Point, neighbours, getY, getX, diagonals)
import Data.Set (Set, member, union, singleton, notMember, insert, empty, size, toList)
import Data.Map (Map, findWithDefault, (!), keys)

floodFill :: Map Point Char -> Set Point -> Point -> Set Point
floodFill grid visited p = foldl (floodFill grid) visited' ns
  where ns = filter (`notMember` visited) $ filter same $ map (p +) neighbours
        visited' = foldl (flip insert) visited ns
        c = grid ! p
        same p' = findWithDefault '.' p' grid == c

toRegions :: Map Point Char -> Set Point -> [Point] -> [Set Point]
toRegions    _       _ [ ] = []
toRegions grid visited (p:ps)
  | p `member` visited = toRegions grid visited ps
  | otherwise = region : toRegions grid (region `union` visited) ps
  where region = floodFill grid (singleton p) p

corners :: Map Point Char -> Set Point -> Point -> Int
corners grid region point =
  case length validN of
    0 -> 4 -- 4 corners, we're alone
    1 -> 2 -- 2 corners, + at most 2 diagonals (next to neighbour)
    -- if they're opposites, we're counting these on the other side
    -- if they're not
    2 -> if opposites (head validN) (validN !! 1)
            then 0 -- These are counted at the neighbours, if they exist
            else if opp2 `notMember` region then 2 else 1 -- But this one isn't
    3 -> length $ filter (`notMember` region) $ opp3 valid1 valid2 valid3 -- there's two possible diagonals we need to check
    4 -> length $ filter (`notMember` region) $ map (point +) diagonals -- All diagonals should be counted
    _ -> error "Our puzzle went beyond 2 dimensions"

  where validN = validNeighbours grid point
        -- These are lazily used after a length check above
        valid1 = head validN -- first valid neighbour
        valid2 = validN !! 1 -- second valid neighbour
        valid3 = validN !! 2 -- third valid neighbour
        -- If the two neighbours are not opposites, there's a shared interior corner
        opp2 = oppDiag valid1 valid2
        oppDiag a b = point - ((point - a) + (point - b))
        -- If there's three neighbours, there's two interior corners
        opp3 a b c
          | opposites a b = [oppDiag a c, oppDiag b c]
          | opposites a c = [oppDiag a b, oppDiag b c]
          | otherwise = [oppDiag a b, oppDiag a c]

validNeighbours :: Map Point Char -> Point -> [Point]
validNeighbours grid p = validNeighbours'
  where validNeighbours' = filter same $ map (p +) neighbours
        c = grid ! p
        same p' = findWithDefault '.' p' grid == c

opposites :: Point -> Point -> Bool
opposites a b = abs (getX d) == 2 || abs (getY d) == 2
  where d = a - b

price :: Map Point Char -> Set Point -> Int
price grid ps = area * sides'
  where area = size ps
        -- sides' equals the sum of corners, apparently
        sides' = sum $ map (corners grid ps) (toList ps)

solve :: IO ()
solve = do
  grid <- toPointMap id . lines <$> getContents
  let regions = toRegions grid empty $ keys grid
  printf "Regions: %d\n" (sum $ map (price grid) regions)

