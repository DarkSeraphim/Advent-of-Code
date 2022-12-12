module Helpers.Graph (bfs, dijkstraPath, dijkstraPaths, dijkstra, dijkstra', PathResult (dist, parent)) where
import Data.Map (Map, empty, findWithDefault, (!))
import Data.Set (Set, findMin, union, deleteFindMin, fromList, notMember, member, singleton)
import qualified Data.Set as S
import qualified Data.Map as M
import Debug.Trace (trace)

data PathResult a = PathResult {dist :: Int, parent :: a} deriving Show

rebuildPath :: Ord a => a -> Map a (PathResult a) -> Maybe [a]
rebuildPath cur res = do
  res' <- M.lookup cur res
  let p = parent res'
  if cur == p then Just [cur] else (cur :) <$> rebuildPath p res

dijkstraPath :: Ord a => Show a => Map (a, a) Int -> Map a [a] -> a -> a -> Maybe [a]
dijkstraPath weights edges start end = rebuildPath end res
  where res = dijkstra' (\k -> findWithDefault maxBound k weights) edges start

dijkstraPaths :: Ord a => Show a => Map (a, a) Int -> Map a [a] -> [a] -> a -> [Maybe [a]]
dijkstraPaths weights edges starts end = map (`rebuildPath` res) starts
  where res = dijkstra' (\k -> findWithDefault maxBound k weights) edges end

dijkstra :: Ord a => Show a => Map (a, a) Int -> Map a [a] -> a -> Map a (PathResult a)
dijkstra weights = dijkstra' (\k -> findWithDefault maxBound k weights)

dijkstra' :: Ord a => Show a => ((a, a) -> Int) -> Map a [a] -> a -> Map a (PathResult a)
dijkstra' wfunc edges start = res
  where res = dijkstra'' wfunc edges S.empty (singleton (0, start, start))

-- | This implementation currently doesn't stop at the end node, but computes the full graph
dijkstra'' :: Ord a => ((a, a) -> Int) -> Map a [a] -> Set a -> Set (Int, a, a) -> Map a (PathResult a)
dijkstra'' wfunc edges visited queue
  | null queue = empty
  | cur `member` visited = dijkstra'' wfunc edges visited queue'
  | otherwise = M.insert cur (PathResult w parent) $ dijkstra'' wfunc edges visited' queue''
  where ((w, cur, parent), queue') = deleteFindMin queue
        visited' = S.insert cur visited
        newEdges = filter (`notMember` visited') (findWithDefault [] cur edges)
        queue'' = queue' `union` fromList (map (\v -> (w + wfunc (cur, v), v, cur)) newEdges)

bfs :: Ord a => Show a => Map a [a] -> a -> a -> Maybe [a]
bfs edges start end = rebuildPath end $ dijkstra' (const 1) edges start
