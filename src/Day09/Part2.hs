module Day09.Part2 (solve) where
    import Text.ParserCombinators.Parsec (digit, GenParser, endBy1, parse)
    import Helpers.Input (readInt, orFail)
    import Control.Applicative (some)
    import Text.Parsec (endOfLine)
    import Text.Printf (printf)
    import Data.Map (Map, fromList, findWithDefault)
    import Data.Foldable (find)
    import Data.Maybe (isNothing)
    import Debug.Trace (trace)
    import Data.Set (Set, member, insert, notMember, empty)
    import Data.List (sort)

    type Point = (Int, Int)

    parseInput :: GenParser Char st [[Int]]
    parseInput = endBy1 (some $ readInt . (: []) <$> digit) endOfLine

    neighbours = [(0, 1), (1, 0), (0, -1), (-1, 0)]

    add :: Point -> Point -> Point
    add (x, y) (a, b) = (x + a, y + b)

    add' :: (Point, Int) -> Point -> (Point, Int)
    add' (p, v) a = (add p a, v)


    isLow :: Map Point Int -> (Point, Int) -> Bool
    isLow grid (point, value) = lower
      where lower = isNothing $ find (\x -> findWithDefault 10 x grid <= value) $ map (add point) neighbours

    getNonNine :: (Point, Int) -> [Point]
    getNonNine (point, 9) = []
    getNonNine (point, _) = [point]

    flood' :: Map Point Int -> Set Point -> Point -> Set Point
    flood' grid visited point
      | doVisit   = visited''
      | otherwise = visited
      where doVisit = notMember point visited && findWithDefault 9 point grid /= 9
            visited' = insert point visited
            visited'' = foldl (flood' grid) visited' $ map (add point) neighbours

    flood :: Map Point Int -> Point -> Set Point
    flood grid = flood' grid empty

    getPoints :: (Point, Int) -> Point
    getPoints (a, b) = a

    solve = do
        input <- orFail . parse parseInput "input" =<< getContents
        let points = concat $ zipWith (\ x ys -> map (\ (y, v) -> ((x, y), v)) ys) [0..] (map (zip [0..]) input)
        let grid = fromList points
        let lows = filter (isLow grid) points
        let basins = map ((length . flood grid) . getPoints) lows
        printf "Risk sum is %d" $ product $ take 3 $ reverse (sort basins)
