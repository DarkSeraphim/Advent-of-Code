module Day07.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Data.Map (Map, keys, member, (!), insert, empty)
import qualified Data.Map as Map
import Helpers.List (head')
import Helpers.Point (Point, newPoint)

data Cell = Start | Empty | Split deriving Eq

readCell :: Char -> Cell
readCell 'S' = Start
readCell '^' = Split
readCell  _  = Empty

findFirst :: Eq v => Map k v -> v -> k
findFirst m val = head' $ keys $ Map.filter (==val) m

down :: Point
down = newPoint 0 1

left :: Point
left = newPoint (-1) 0

right :: Point
right = newPoint 1 0

walk :: Map Point Cell -> Point -> Map Point Int -> (Map Point Int, Int)
walk grid pos visited = case nextCell of
                          Nothing -> (visited, 1)
                          Just Start -> walk grid next visited
                          Just Empty -> walk grid next visited
                          Just Split -> 
                            if next `member` visited
                               then (visited, visited ! next)
                               else (visited', res)
  where next = pos + down
        nextCell = Map.lookup next grid
        l = next + left
        r = next + right
        rec = walk grid
        (visL, resL) = rec l visited
        (visR, resR) = rec r visL
        res = resL + resR
        visited' = insert next res visR 
        

solve :: IO ()
solve = do
  grid <- parseGrid readCell
  let start = findFirst grid Start
  let (_, timelines) = walk grid start empty
  printf "We have %d timelines\n" timelines 
