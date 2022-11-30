{-# LANGUAGE TupleSections #-}
module Day05.Part2 (solve) where
    import Text.ParserCombinators.Parsec.Prim (GenParser)
    import Text.ParserCombinators.Parsec (string, char, digit, endBy1, parse)
    import Helpers.Input (readInt, orFail)
    import Control.Applicative (some)
    import Text.Parsec (endOfLine)
    import Text.Printf (printf)
    import Data.Map (Map, empty, insertWith, size, filter)
    import Debug.Trace (trace)
    import Helpers.Parsec (number)
    
    type Point = (Int, Int)
    type Line = (Point, Point)

    parseInput :: GenParser Char st [Line]
    parseInput = endBy1 line endOfLine

    line :: GenParser Char st Line
    line = do
        start <- point
        string " -> "
        end <- point
        return (start, end)

    point :: GenParser Char st Point
    point = do
        x <- number
        char ','
        y <- number
        return (x, y)
 
    addAll :: Map Point Int -> [Point] -> Map Point Int
    addAll = foldl (\map point -> insertWith (+) point 1 map)

    toRange :: Int -> Int -> [Int]
    toRange a b
      | a <= b = [a..b]
      | a > b = reverse [b..a]
      | otherwise = error "Wat"

    addPoints :: Map Point Int -> Line -> Map Point Int
    addPoints visits ((x1, y1), (x2, y2))
      | x1 == x2 = addAll visits $ map (x1,) [minY..maxY]
      | y1 == y2 = addAll visits $ map (,y1) [minX..maxX]
      -- Line needs to be perfect diagonal
      | (maxX - minX) == (maxY - minY) = addAll visits $ zip (toRange x1 x2) (toRange y1 y2)
      | otherwise = visits
        where minX = min x1 x2 
              maxX = max x1 x2 
              minY = min y1 y2 
              maxY = max y1 y2 

    solve = do
        lines <- orFail . parse parseInput "Whoops" =<< getContents
        let visits = foldl addPoints empty lines
        let sol = size $ Data.Map.filter (> 1) visits
        printf "Overlapping points: %d\n" sol
