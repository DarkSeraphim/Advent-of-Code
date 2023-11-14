{-# LANGUAGE NamedFieldPuns #-}
module Day20.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (endOfLine, endBy1)
import Helpers.Parsec (number, parseInput, numberInteger)
import Data.Map (empty, insert, Map, (!), keys)
import Helpers.List (cycleN)

type Value = (Int, Integer)
data Node = Node {prv :: Value, value :: Value, nxt :: Value} deriving Show
type Links = Map Value Node

newNode :: (Value, Value, Value) -> Node
newNode (value, prv, nxt) = Node { prv, value, nxt}

parse = numberInteger `endBy1` endOfLine

append :: a -> [a] -> [a]
append x xs = reverse $ x : reverse xs

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

reconstructList :: Value -> Value -> Links -> [Value]
reconstructList first cur nodes
  | next == first = [cur]
  | otherwise = cur : reconstructList first next nodes
  where next = nxt (nodes ! cur)

setNext :: Node -> Value -> Node
setNext n v = n {nxt = v}
setPrev :: Node -> Value -> Node
setPrev n v = n {prv = v}

remove :: Node -> Links -> Links
remove cur nodes = nodes''
  where prev = nodes ! prv cur
        next = nodes ! nxt cur
        nodes' = insert (value prev) (setNext prev (value next)) nodes
        nodes'' = insert (value next) (setPrev next (value prev)) nodes'

insertBefore :: Node -> Node -> Links -> Links
insertBefore cur tgt nodes = nodes''
  where cur' = setNext cur (value tgt)
        tgt' = setPrev tgt (value cur)
        nodes' = insert (value cur') cur' nodes
        nodes'' = insert (value tgt') tgt' nodes'

insertAfter :: Node -> Node -> Links -> Links
insertAfter cur tgt nodes = nodes''
  where cur' = setPrev cur (value tgt)
        tgt' = setNext tgt (value cur)
        nodes' = insert (value cur') cur' nodes
        nodes'' = insert (value tgt') tgt' nodes'

findNext :: (Node -> Value) -> Links -> Node -> Int -> Node
findNext func nodes cur 0 = cur
findNext func nodes cur moves = findNext func nodes cur' (moves - 1)
  where cur' = nodes ! func cur

findNewNeighbours :: Node -> Links -> (Node, Node)
findNewNeighbours cur nodes
  -- Special case 0, we don't move
  | moves' == 0 = (findNext prv nodes cur 1, findNext nxt nodes cur 1)
  | moves < 0 = (func n 1, n)
  | otherwise = (n, func n 1)
  where moves = snd $ value cur
        moves' = fromInteger $ abs moves `rem` (toInteger $ length nodes - 1)
        func = findNext (if moves < 0 then prv else nxt) nodes -- movement function
        n = func cur moves'

shuffle :: Value -> Links -> Links
shuffle v nodes = nodes'''
  where cur = nodes ! v
        nodes' = remove cur nodes
        (prev, next) = findNewNeighbours cur nodes'
        nodes'' = insertBefore cur next nodes'
        cur' = nodes'' ! v
        nodes''' = insertAfter cur' prev nodes''

solve = do
  nums <- zip [0 :: Int ..] . map (811589153 *) <$> parseInput parse
  let numWithNeighbours = zip3 nums (last nums : nums) (append (head nums) (tail nums))
  let nodes = foldl (\m t -> insert (fst3 t) (newNode t) m) empty numWithNeighbours
  let first = head nums
  let values = reconstructList first first nodes -- This stays
  let nodes' = foldl (flip shuffle) nodes (cycleN 10 values)
  let first = head $ filter (\t -> 0 == snd t) (keys nodes')
  let finalList = map snd $ reconstructList first first nodes'
  let a = map (\n -> toInteger $ cycle finalList !! max 0 n) [1000, 2000, 3000]
  printf "List: %s\n" (show $ sum a)
