{-# LANGUAGE TupleSections #-}
module Day16.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (count, anyChar, sepBy1, string, endOfLine, endBy1, try, option, (<|>))
import Data.Map (Map, empty, insert, (!), toList)
import qualified Data.Map as M
import Data.Set (Set, notMember)
import qualified Data.Set as S
import Helpers.Graph (bfs, PathResult (dist))
import Debug.Trace (trace)
import Data.List (sort, sortOn)
import GHC.Exts (sortWith)
import qualified Data.Ord

type Valve = (String, Int, [String])

valveName :: Parser String
valveName = count 2 anyChar

endpoints :: Parser [String]
endpoints = valveName `sepBy1` string ", "

badText = try (string "; tunnels lead to valves ") <|> string "; tunnel leads to valve "

parseValve :: Parser (String, Int, [String])
parseValve = (,,) <$> (string "Valve " *> valveName <* string " has flow rate=") <*> number <*> (badText *> endpoints)
parseValves :: Parser [Valve]
parseValves = parseValve `endBy1` endOfLine

toMap :: [Valve] -> (Map String Int, Map String [String])
toMap [] = (empty, empty)
toMap ((name, flow, edges):rest) = (insert name flow flows, insert name edges edgesMap)
  where (flows, edgesMap) = toMap rest

buildReducedGraph :: Show a => Ord a => Map a [a] -> [a] -> Map (a, a) Int
buildReducedGraph edges nodes = M.fromList results
  where results = concatMap (\a -> map (\(b, d) -> ((a, b), dist d)) $ toList $ bfs edges a) nodes

data Tunnels = Tunnels {flows :: Map String Int, weights :: Map (String, String) Int, edges :: Map String [String]}
type Time = Int

computeFlow :: Tunnels -> Set String -> Int
computeFlow tunnels open = sum $ S.map (flows tunnels !) open

getTime :: Tunnels -> String -> String -> Int
getTime tunnels cur next = weights tunnels ! (cur, next)

walkTunnels :: Tunnels -> Set String -> String -> Time -> [(Set String, Int)]
walkTunnels tunnels open cur 0 = [(open, computeFlow tunnels open)]
walkTunnels tunnels open cur time = (open, computeFlow tunnels open * time) : res'
  where recurse n = map (\(s, i) -> (s, i + timeItTakes * computeFlow tunnels open)) $ walkTunnels tunnels (S.insert n open) n (time - timeItTakes)
            where timeItTakes = getTime tunnels cur n + 1
        next = edges tunnels ! cur
        -- Only consider unopened ones
        next' = filter (`S.notMember` open) next
        -- and only when we have the time to use them
        next'' = filter (\n -> getTime tunnels cur n + 1 < time) next'
        res = concatMap recurse next''
        res' = case res of
                [] -> [(open, computeFlow tunnels open * time)]
                results -> sortOn (Data.Ord.Down . snd) results

startNode = "AA"

allPairs :: [a] -> [a] -> [(a, a)]
allPairs l r = concatMap (\a -> map (a,) r) l


solve = do
  (flows, edges) <- toMap <$> parseInput parseValves
  let importantNodes = startNode : M.keys (M.filter (>0) flows)
  -- Build a new fully connected graph where all 0 nodes (except start) are excluded
  let weights = buildReducedGraph edges importantNodes
  -- Rebuild edges map from reduced graph weights
  let edges' = foldl (\m (s, e) -> M.insertWith (++) s [e] m) M.empty $ M.keys weights
  let edges'' = M.map (filter (`S.member` S.fromList importantNodes)) edges'
  let tunnels = Tunnels flows weights edges''
  let res = S.toList $ S.fromList $ sortOn (Data.Ord.Down . snd) $ walkTunnels tunnels (S.singleton "AA") startNode 26
  -- I know 2651 was not enough, and the max was 1700, so we need at least 900 to beat it
  let res''' = filter (\s -> snd s > 900) res
  let res' = filter (\((sa, sai), (sb, sbi)) -> S.singleton "AA" == (sa `S.intersection` sb)) (allPairs res''' res''')
  let res'' = last (sortOn (\(a, b, c) -> a) (map (\((sa, sai), (sb, sbi)) -> (sai + sbi, sa, sb)) res'))
  printf "Found %s flow\n" (show res'')
