module Helpers.Graph (bfs, bfsPath, bfsPathCond, dijkstraPath, dijkstraPaths, dijkstra, dijkstra', PathResult (dist, parent)) where
import Data.Maybe (mapMaybe)
import Data.Map (Map, empty, findWithDefault)
import Data.Set (Set, union, deleteFindMin, fromList, notMember, member, singleton)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (sortOn)

data PathResult a = PathResult {dist :: Int, parent :: a} deriving Show

rebuildPath :: Ord a => a -> Map a (PathResult a) -> Maybe [a]
rebuildPath cur res = do
  res' <- M.lookup cur res
  let p = parent res'
  if cur == p then Just [cur] else (cur :) <$> rebuildPath p res

dijkstraPath :: Ord a => Map (a, a) Int -> Map a [a] -> a -> a -> Maybe [a]
dijkstraPath weights edges start end = rebuildPath end res
  where res = dijkstra' (\k -> findWithDefault maxBound k weights) edges start (== end)

dijkstraPaths :: Ord a => Map (a, a) Int -> Map a [a] -> [a] -> a -> [Maybe [a]]
dijkstraPaths weights edges starts end = map (`rebuildPath` res) starts
  where res = dijkstra' (\k -> findWithDefault maxBound k weights) edges end (const False)

dijkstra :: Ord a => Map (a, a) Int -> Map a [a] -> a -> Map a (PathResult a)
dijkstra weights edges start = dijkstra' (\k -> findWithDefault maxBound k weights) edges start (const False)

dijkstra' :: Ord a => ((a, a) -> Int) -> Map a [a] -> a -> (a -> Bool) -> Map a (PathResult a)
dijkstra' wfunc edges start done = res
  where res = dijkstra'' wfunc edges S.empty (singleton (0, start, start)) done

-- | This implementation currently doesn't stop at the end node, but computes the full graph
dijkstra'' :: Ord a => ((a, a) -> Int) -> Map a [a] -> Set a -> Set (Int, a, a) -> (a -> Bool) -> Map a (PathResult a)
dijkstra'' wfunc edges visited queue done
  | null queue = empty
  | cur `member` visited = dijkstra'' wfunc edges visited queue' done
  | done cur = M.singleton cur (PathResult w parent')
  | otherwise = M.insert cur (PathResult w parent') $ dijkstra'' wfunc edges visited' queue'' done
  where ((w, cur, parent'), queue') = deleteFindMin queue
        visited' = S.insert cur visited
        newEdges = filter (`notMember` visited') (findWithDefault [] cur edges)
        queue'' = queue' `union` fromList (map (\v -> (w + wfunc (cur, v), v, cur)) newEdges)

bfs :: Ord a => Map a [a] -> a -> Map a (PathResult a)
bfs edges start = dijkstra' (const 1) edges start (const False)

bfsPath :: Ord a => Map a [a] -> a -> a -> Maybe [a]
bfsPath edges start end = rebuildPath end res
  where res = dijkstra' (const 1) edges start (== end)

bfsPathCond :: Ord a => Map a [a] -> a -> (a -> Bool) -> Maybe [a]
bfsPathCond edges start endCond
  | null paths = Nothing
  | otherwise = Just $ minimum $ sortOn length paths
  where res = dijkstra' (const 1) edges start endCond
        ends = filter endCond $ M.keys edges
        paths = mapMaybe (`rebuildPath` res) ends
