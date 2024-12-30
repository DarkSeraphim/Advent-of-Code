module Day23.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec (noneOf, many1, char, endBy1, endOfLine)
import Data.Set ( Set, singleton, union, intersection, unions, member )
import Data.Map (Map, unionWith, (!), keys, fromListWith)
import Control.Arrow (Arrow(second))
import Data.Tuple (swap)

import qualified Data.Set as S

node :: Parser String
node = many1 (noneOf "-\r\n")

edge :: Parser (String, String)
edge = (,) <$> node <* char '-' <*> node

edges :: Parser [(String, String)]
edges = edge `endBy1` endOfLine

toEdgeMap :: (Ord a, Ord b) => [(a, b)] -> Map a (Set b)
toEdgeMap l = fromListWith union $ map (second singleton) l

triGraphs :: Ord a => Map a (Set a) -> a -> Set (Set a)
triGraphs graph start = S.map (S.insert start) secondary
  where edges' = graph ! start
        secondary = unions $ S.map (\e -> S.map (S.insert e . S.singleton) (edges' `intersection` (graph ! e))) edges'

connectTriGraph :: Ord a => Map a (Set a) -> Set a -> Set a -> Maybe (Set a)
connectTriGraph graph as bs = if all connectedToB as then Just (as `union` bs) else Nothing
  where connectedToB a = all (\b -> a `member` (graph ! b)) bs

max' :: Set a -> Set a -> Set a
max' a b = if length a > length b then a else b

attemptConnectingTriGraphs :: Ord a => Map a (Set a) -> Set a -> [Set a] -> Set a
attemptConnectingTriGraphs _ s [] = s
attemptConnectingTriGraphs g s (a:as) =
  case connected of
    Just s' -> max' (attemptConnectingTriGraphs g s' as) (attemptConnectingTriGraphs g s as)
    Nothing -> attemptConnectingTriGraphs g s as
  where connected = connectTriGraph g s a

byKey :: Set (Set String) -> String -> (String, Set (Set String))
byKey trigraphs leaf = (leaf, S.filter (leaf `S.member`) trigraphs)

solve :: IO ()
solve = do
  edges' <- parseInput edges
  let graph = unionWith union (toEdgeMap edges') (toEdgeMap (map swap edges'))
      leaves = filter (\k -> take 1 k == "t") $ keys graph
      trigraphs = unions $ map (triGraphs graph) leaves
      sizes = map (second S.size . byKey trigraphs) leaves
  printf "By size: %s" (show sizes)
  printf "Largest fully connected graph is %d in size, and has the password '%s'\n" (length trigraphs) ""

