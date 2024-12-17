{-# LANGUAGE TupleSections #-}
module Day16.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Helpers.Point (Point, neighbours, getX, getY, newPoint)
import Data.Map (toList, fromList, Map, (!))
import Data.Set (empty, singleton, Set, insert, unions)
import Helpers.Graph (dijkstra''', PathResult(dist), rebuildAllPaths)
import Helpers.Map ((!?))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import qualified Data.Set as S

data Dir = N | S | E | W deriving (Ord, Eq, Show)
type Key = (Point, Dir)

dirs :: [Dir]
dirs = [N, S, E, W]

addDirections :: (Point, Char) -> [((Point, Dir), Char)]
addDirections (p, c) = map ((, c) . (p,)) dirs

wfunc :: (Key, Key) -> Int
wfunc ((_, da), (_, db)) = 1 + extra
  where extra = if da /= db then 1000 else 0

findDir :: Point -> Point -> Dir
findDir a b
  | dx ==  1 = E
  | dx == -1 = W
  | dy ==  1 = S
  | dy == -1 = N
  | otherwise = error "Not a direction"
  where dx = getX b - getX a
        dy = getY b - getY a

efunc :: Map Key Char -> Key -> [Key]
efunc grid (p, _) = map (\p' -> (p', findDir p p')) next
  where next = filter ((/= '#'). lup) $ map (p +) neighbours
        lup :: Point -> Char
        lup p' = snd . h $ M.toList $ M.filterWithKey (\(p'', _) _ -> p'' == p') grid
        h :: [((Point, Dir), Char)] -> ((Point, Dir), Char)
        h [] = ((newPoint 0 0, N), '#')
        h (a:as) = a

done :: Map Key Char -> Key -> Bool
done grid key = grid ! key == 'E'


floodFill :: Int -> Map Key Char -> Set Key -> Key -> Int -> Set Key
floodFill target grid visited cur@(p, _) w
  | w > target =  empty
  | cur `S.member` visited = empty
  | done grid cur && w == target = insert cur visited
  | otherwise = trace (show e) $ foldl (\s e' -> s ` S.union` rec e') empty e
  where rec cur'@(p', _) = floodFill target grid (insert cur visited) cur' (w + wfunc (cur, cur'))
        e = efunc grid cur

findAll :: Map Key (PathResult Key) -> Set Key -> Key -> Set Key
findAll grid visited start = visited
  where 

solve :: IO ()
solve = do
  grid <- fromList . concatMap addDirections . toList <$> parseGrid id
  let (start, _) = head $ filter (\((_, d), v) -> d == E && v == 'S') $ toList grid
  let ((endPoint, _), _) = head $ filter (\(_, v) -> v == 'E') $ toList grid

  let path = dijkstra''' wfunc (efunc grid) empty (singleton (0, start, start)) (done grid)
  let result = head $ mapMaybe ((`M.lookup` path) . (endPoint,)) dirs

  let full = dijkstra''' wfunc (efunc grid) empty (singleton (0, start, start)) (const False)
  let grid' = M.filterWithKey (\k _ -> k `M.member` full) grid
  printf "Nodes visited: %d, nodes total: %d\n" (length full) (length grid)
  printf "%s\n" (show $ map (`M.lookup` full) $ map (newPoint 3 12,) dirs)
  let ends = map (endPoint,) dirs
  let bestSeats = S.fromList $ map fst $ concat $ concatMap (rebuildAllPaths full) ends

  --let bestSeats =  S.map fst $ floodFill (dist result) grid' empty start 0

  printf "Number of best seats: %d\n" (length bestSeats)

