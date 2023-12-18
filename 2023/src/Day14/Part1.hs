{-# LANGUAGE TupleSections #-}
module Day14.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGridMaybe)
import Data.Map (keys, fromList, union, Map, filter, (!?))
import Helpers.Point (getX, newPoint, Point, getY)
import Prelude hiding (filter)

data Rock = Round | Square | Not deriving (Eq, Show)

fromChar :: Char -> Maybe Rock
fromChar '#' = Just Square
fromChar 'O' = Just Round
fromChar  _  = Just Not

delta :: Point
delta = newPoint 0 1

countTillNext :: Map Point Rock -> Point -> Int
countTillNext rocks start = case next of
                              Just Round -> 1 + recurse
                              Just Square -> 0
                              Just Not -> recurse
                              Nothing -> 0
  where nextPos = start + delta
        next = rocks !? (start + delta)
        recurse = countTillNext rocks nextPos


findSupports :: Map Point Rock -> [(Point, Int)]
findSupports rocks = map (\s -> (s, countTillNext rocks s)) squares
  where squares = keys $ filter (==Square) rocks

score :: Int -> (Point, Int) -> Int
score totalY (point, num) = sum $ map (\x -> 1 + totalY - x) [minY .. maxY]
  where minY = getY point + 1
        maxY = minY + num - 1 -- range is inclusive

solve :: IO ()
solve = do
  rocks <- parseGridMaybe fromChar
  let x = map getX $ keys rocks
  let maxY = maximum $ map getY $ keys rocks
  let bedrock = fromList $ map ((, Square) . (`newPoint` 0)) [minimum x .. maximum x]
  let supports = findSupports (rocks `union` bedrock)

  printf "Rock weight is %d\n" (sum $ map (score maxY) supports)
