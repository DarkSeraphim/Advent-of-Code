module Day09.Part1 (solve) where
    import Text.ParserCombinators.Parsec (digit, GenParser, endBy1, parse)
    import Helpers.Input (readInt, orFail)
    import Control.Applicative (some)
    import Text.Parsec (endOfLine)
    import Text.Printf (printf)
    import Data.Map (Map, fromList, findWithDefault)
    import Data.Foldable (find)
    import Data.Maybe (isNothing)
    import Debug.Trace (trace)
    
    type Point = (Int, Int)

    parseInput :: GenParser Char st [[Int]]
    parseInput = endBy1 (some $ readInt . (: []) <$> digit) endOfLine

    neighbours = [(0, 1), (1, 0), (0, -1), (-1, 0)]

    add :: Point -> Point -> Point
    add (x, y) (a, b) = (x + a, y + b)

    computeRisk :: Map Point Int -> (Point, Int) -> Int
    computeRisk grid (point, value)
      | lower     = value + 1
      | otherwise = 0
      where lower = isNothing $ find (\x -> findWithDefault 10 x grid <= value) $ map (add point) neighbours

    solve = do
        input <- (orFail . parse parseInput "input") =<< getContents
        let points = concat $ zipWith (\ x ys -> map (\ (y, v) -> ((x, y), v)) ys) [0..] (map (zip [0..]) input)
        let grid = fromList points
        
        printf "Risk sum is %d" $ sum (map (computeRisk grid) points)
