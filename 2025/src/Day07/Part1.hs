module Day07.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGrid)
import Data.Map (Map, keys)
import qualified Data.Map as Map
import Helpers.List (head')
import Data.Set (Set, member, empty, insert, size)
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

walk :: Map Point Cell -> Point -> Set Point -> Set Point
walk grid pos visited = case nextCell of
                          Nothing -> visited
                          Just Start -> walk grid next visited
                          Just Empty -> walk grid next visited
                          Just Split -> 
                            if next `member` visited
                               then visited
                               else rec r (rec l (insert next visited))
  where next = pos + down
        nextCell = Map.lookup next grid
        l = next + left
        r = next + right
        rec = walk grid
        

solve :: IO ()
solve = do
  grid <- parseGrid readCell
  let start = findFirst grid Start
  let splits = walk grid start empty
  printf "We split %d times\n" (size splits) 
