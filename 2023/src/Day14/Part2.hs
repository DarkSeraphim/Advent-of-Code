{-# LANGUAGE TupleSections #-}
module Day14.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGridMaybe)
import Data.Map (keys, fromList, union, Map, filter, (!?), insert, member, (!), singleton)
import Helpers.Point (getX, newPoint, Point, getY)
import Prelude hiding (filter)
import Control.Monad (join)
import Data.List (intercalate)

data Rock = Round | Square deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe Rock
fromChar '#' = Just Square
fromChar 'O' = Just Round
fromChar  _  = Nothing

toChar :: Rock -> Char
toChar Square = '#'
toChar Round = 'O'

countTillNext' :: (Point -> Bool) -> Map Point Rock -> Point -> Point -> Int
countTillNext' inside rocks delta start
  | not $ inside nextPos = 0
  | otherwise = case next of
                  Just Round -> 1 + recurse
                  Just Square -> 0
                  Nothing -> recurse
  where nextPos = start + delta
        next = rocks !? nextPos
        recurse = countTillNext' inside rocks delta nextPos


countTillNext :: Map Point Rock -> Point -> Point -> Int
countTillNext rocks = countTillNext' f rocks
  where f pos = minX <= x && x <= maxX && minY <= y && y <= maxY
          where x = getX pos
                y = getY pos
        xs = map getX $ keys rocks
        ys = map getY $ keys rocks
        minX = minimum xs
        maxX = maximum xs
        minY = minimum ys
        maxY = maximum ys

findSupports :: Map Point Rock -> Point -> [(Point, Int)]
findSupports rocks delta = map (\s -> (s, countTillNext rocks delta s)) squares
  where squares = keys $ filter (==Square) rocks

toPoint :: Int -> Point
toPoint = join newPoint

expandWithRounds :: Point -> Point -> Int -> [(Point, Rock)]
expandWithRounds dir point i = (point, Square) : rounds
  where rounds = map ((, Round) . (point +) . (dir * ) . toPoint) [1..i]

rebuildGrid :: [(Point, Int)] -> Point -> Map Point Rock
rebuildGrid supports dir = map'
  where map' = fromList $ concatMap (uncurry (expandWithRounds inv)) supports
        inv = dir

cycleRocksDir :: Map Point Rock -> Point -> Map Point Rock
cycleRocksDir rocks dir = rebuildGrid (findSupports rocks dir) dir

cycleDirs :: [Point]
cycleDirs = [newPoint 0 1, newPoint 1 0, newPoint 0 (-1), newPoint (-1) 0]


cycleRocks :: Map Point Rock -> Map Point Rock
cycleRocks rocks = foldl cycleRocksDir rocks cycleDirs

iterateRocks :: Int -> Map Point Rock -> Map (Map Point Rock) Int -> Map Point Rock
iterateRocks i rocks seen
  | i == (1000000000 - 1) = rocks'
-- Force the last 100 iterations
  | i > (1000000000 - 100) = iterateRocks (i + 1) rocks' seen
  | rocks' `member` seen = iterateRocks (i + loopSize * ((1000000000 - i) `div` loopSize)) rocks' seen
  | i > 1000000000 = error "Too many loops"
  | i > 0 = iterateRocks (i + 1) rocks' seen'
  | otherwise = error "oops"
  where rocks' = cycleRocks rocks
        seen' = insert rocks' i seen
        seenAt = seen ! rocks'
        loopSize = i - seenAt



score :: Int -> Point -> Int
score maxY p = maxY - getY p + 1

display :: Map Point a -> (a -> Char) -> Char -> String
display grid f def = intercalate "\n" s
  where s = map (\y -> map (\x -> maybe def f $ grid !? newPoint x y) [minX..maxX]) [minY..maxY]
        xs = map getX $ keys grid
        ys = map getY $ keys grid
        minX = minimum xs
        minY = minimum ys
        maxX = maximum xs
        maxY = maximum ys

solve :: IO ()
solve = do
  rocks <- parseGridMaybe fromChar
  let x = map getX $ keys rocks
  let y = map getY $ keys rocks
  let minX = minimum x
  let maxX = maximum x
  let minY = minimum y
  let maxY = maximum y
  let bedrock = fromList $ concat $ [
                                      map ((, Square) . (`newPoint` 0)) [minX .. maxX],
                                      map ((, Square) . (`newPoint` (maxY + 1))) [minX .. maxX],
                                      map ((, Square) . newPoint 0) [minY .. maxY],
                                      map ((, Square) . newPoint (maxX + 1)) [minY .. maxY]
                                    ]
  let startGrid = rocks `union` bedrock
  let loops = 1 :: Int
  let supports = iterateRocks loops startGrid (singleton startGrid 0)
  let result = supports
  printf "%s\n" (display result toChar '.')
  printf "Rock weight is %d\n" (sum $ map (score maxY) (keys $ filter (==Round) result))
