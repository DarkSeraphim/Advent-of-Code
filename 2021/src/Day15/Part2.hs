{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Day15.Part2 (solve) where
    import Text.Parsec.Char (endOfLine)
    import Text.ParserCombinators.Parsec ( GenParser, endBy1, digit )
    import Helpers.Input (orFail, readInt)
    import Text.ParserCombinators.Parsec.Prim (parse)
    import Text.Printf (printf)
    import Control.Applicative (some)
    import Data.Map (fromList, Map, findWithDefault, (!), empty, member, singleton, notMember, insert, keys, lookup)
    import Data.List (sort)
    import Debug.Trace (trace)

    type Point = (Int, Int)

    parseInput :: GenParser Char st [[Int]]
    parseInput = some parseNumber `endBy1` endOfLine

    parseNumber :: GenParser Char st Int
    parseNumber = readInt . (:[]) <$> digit

    wrap :: Int -> Int -> Int
    wrap v i
      | w > 9     = w - 9
      | otherwise = w
      where w = v + i

    addWithWrap :: Int -> [[Int]] -> [[Int]]
    addWithWrap 0 grid = grid
    addWithWrap i grid = map (map (wrap i)) grid

    expand' :: [[Int]] -> [[[Int]]]
    expand' gridList = zipWith addWithWrap [0..4] (replicate 5 gridList)

    concatRow :: [[[Int]]] -> [[Int]]
    concatRow [] = []
    concatRow rows
      | all null rows = []
      | otherwise = (concatMap head rows) : (concatRow $ map tail rows)

    expand :: [[Int]] -> [[Int]]
    expand gridList = concat horiz'
        where horiz = expand' gridList
              horiz' = map concatRow $ map expand' horiz

    neighbours :: [Point]
    neighbours = [(0, 1), (1, 0), (0, -1), (-1, 0)]

    add :: Point -> Point -> Point
    add (a, b) (c, d) = (a + c, b + d)

    getNeighbours :: Map Point Int -> Point -> [(Int, Point)]
    getNeighbours grid point = zip risks next
        where next = filter (`member` grid) $ map (add point) neighbours
              risks = map (getRisk grid) next

    addDist :: Int -> (Int, Point) -> (Int, Point)
    addDist base (weight, point) = (weight + base, point)

    lowerCost :: Map Point Int -> (Int, Point) -> Bool
    lowerCost dist (cost, point) = findWithDefault maxBound point dist > cost

    findPath :: Map Point Point -> Point -> Point -> [Point]
    findPath prev start end = end : case Data.Map.lookup end prev of
        Just next -> findPath prev start next
        Nothing -> []

    dijkstra' :: Map Point Int -> Point -> [(Int, Point)] -> Map Point Int -> Map Point Point -> Maybe (Map Point Point)
    dijkstra' grid end [] dist prev = Nothing
    dijkstra' grid end ((base, start):rest) dist prev
      | start == end = Just prev
      | otherwise = dijkstra' grid end rest' dist' prev'
      where neighbours = map (addDist base) $ getNeighbours grid start
            neighbours' = filter (lowerCost dist) neighbours
            neighbours'' = map snd neighbours'
            rest' = sort (neighbours' ++ filter (\x -> snd x `notElem` neighbours'') rest)
            prev' = foldl (\p next -> insert next start p) prev $ map snd neighbours'
            dist' = foldl (\d (cost, next) -> insert next cost d) dist neighbours'

    dijkstra :: Map Point Int -> Point -> Point -> [Point]
    dijkstra grid start end = case prev of
            Just prev -> findPath prev start end
            Nothing -> []
        where prev = dijkstra' grid end [(0, start)] (singleton start 0) empty

    getRisk :: Map Point Int -> Point -> Int
    getRisk map point = map ! point

    solve = do
        input <- orFail . parse parseInput "Input" =<< getContents
        -- let grid' = fromList $ concat $ concat $ zipWith (\ y row -> zipWith (\ x value -> [((x, y), value)]) [0 .. ] row) [0..] input
        let input' = expand input
        let grid = fromList $ concat $ concat $ zipWith (\ y row -> zipWith (\ x value -> [((x, y), value)]) [0 .. ] row) [0..] input'
        -- printf "Grid: %s" (show input')
        let start = minimum $ keys grid
        let end = maximum $ keys grid
        printf "Lowest risk path is %d" (sum $ map (getRisk grid) $ init (dijkstra grid start end))
