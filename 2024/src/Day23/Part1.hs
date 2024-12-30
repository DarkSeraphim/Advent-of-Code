module Day23.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec (noneOf, many1, char, endBy1, endOfLine)
import Data.Set ( Set, singleton, union, intersection, unions )
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

solve :: IO ()
solve = do
  edges' <- parseInput edges
  let graph = unionWith union (toEdgeMap edges') (toEdgeMap (map swap edges'))
      trigraphs = unions $ map (triGraphs graph) (filter (\k -> take 1 k == "t") $ keys graph)
  printf "We have %d fully connected trigraphs\n" (length trigraphs)
