{-# LANGUAGE TupleSections #-}
module Day16.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Helpers.Point (Point, neighbours, getX, getY)
import Data.Map (toList, fromList, Map, (!))
import Data.Set (empty, singleton)
import Helpers.Graph (dijkstra''', PathResult(dist))
import Helpers.Map ((!?))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)

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
  where next = filter ((/= '#'). (grid !?) . (, '#') . (, N)) $ map (p +) neighbours

done :: Map Key Char -> Key -> Bool
done grid key = grid ! key == 'E'

solve :: IO ()
solve = do
  grid <- fromList . concatMap addDirections . toList <$> parseGrid id
  let (start, _) = head $ filter (\((_, d), v) -> d == E && v == 'S') $ toList grid
  let ((endPoint, _), _) = head $ filter (\(_, v) -> v == 'E') $ toList grid

  let path = dijkstra''' wfunc (efunc grid) empty (singleton (0, start, start)) (done grid)

  let result = head $ mapMaybe ((`M.lookup` path) . (endPoint,)) dirs
  printf "Weight of the best path is %d\n" (dist result)
