module Helpers.Graph (bfs, dijkstra, dijkstra', PathResult) where
import Data.Map (Map, empty, findWithDefault, (!))
import Data.Set (Set, findMin, union, deleteFindMin, fromList, notMember, member, singleton)
import qualified Data.Set as S
import qualified Data.Map as M

data PathResult a = PathResult {dist :: Int, parent :: a}

rebuildPath :: Ord a => a -> Map a (PathResult a) -> [a]
rebuildPath cur res
  | cur == p  = [cur]
  | otherwise = cur : rebuildPath p res
  where res' = res ! cur
        p = parent res'

dijkstra :: Ord a => Map (a, a) Int -> Map a [a] -> a -> a -> [a]
dijkstra weights = dijkstra' (\k -> findWithDefault maxBound k weights)

dijkstra' :: Ord a => ((a, a) -> Int) -> Map a [a] -> a -> a -> [a]
dijkstra' wfunc edges start end = rebuildPath end res
  where res = dijkstra'' wfunc edges S.empty (singleton (0, start, start))

-- | This implementation currently doesn't stop at the end node, but computes the full graph
dijkstra'' :: Ord a => ((a, a) -> Int) -> Map a [a] -> Set a -> Set (Int, a, a) -> Map a (PathResult a)
dijkstra'' wfunc edges visited queue
  | null queue = empty
  | cur `member` visited = dijkstra'' wfunc edges visited queue'
  | otherwise = M.insert cur (PathResult w parent) $ dijkstra'' wfunc edges visited' queue'
  where ((w, cur, parent), queue') = deleteFindMin queue
        visited' = S.insert cur visited
        newEdges = filter (`notMember` visited') (findWithDefault [] cur edges)
        queue'' = queue' `union` fromList (map (\v -> (w + wfunc (cur, v), v, cur)) newEdges)

bfs :: Ord a => Map a [a] -> a -> a -> [a]
bfs = dijkstra' (const 1)
