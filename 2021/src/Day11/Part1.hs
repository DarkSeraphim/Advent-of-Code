module Day11.Part1 (solve) where
    import Text.ParserCombinators.Parsec (GenParser, endBy1, digit)
    import Text.Parsec.Char (endOfLine)
    import Text.Printf (printf)
    import Helpers.Input (orFail, readInt)
    import Text.Parsec (parse)
    import Control.Applicative (some)
    import Data.Map (Map, fromList, map, filter, keys, insert, adjust, findWithDefault)
    import Data.List (delete, map)

    type Point = (Int, Int)

    parseInput :: GenParser Char st [[Int]]
    parseInput = endBy1 parseLine endOfLine

    parseLine :: GenParser Char st [Int]
    parseLine = some parseNumber

    parseNumber :: GenParser Char st Int
    parseNumber = readInt . (: []) <$> digit

    mapRow :: Int -> [Int] -> [(Point, Int)]
    mapRow y = zipWith (\x v -> ((x, y), v)) [0..]

    add :: Point -> Point -> Point
    add (a, b) (c, d) = (a + c, b + d)

    neighbours :: [Point]
    neighbours = delete (0,0) $ concatMap (zip [-1..1] . cycle . (: [])) [-1..1]

    not10 :: Map Point Int -> Point -> Bool
    not10 grid point = findWithDefault 10 point grid /= 10

    dfs :: Map Point Int -> Point -> Map Point Int
    dfs grid point = foldl dfs grid' ns'
        where ns = Prelude.filter (not10 grid) $ Data.List.map (add point) neighbours
              grid' = foldl (flip (adjust (+ 1))) grid ns
              -- Now that we bumped the values, we should recurse if they're 10
              ns' = Prelude.filter (not . not10 grid') ns

    reset :: Int -> Int
    reset 10 = 0
    reset n = n

    simulate :: Int -> Map Point Int -> Int
    simulate 0 grid = 0
    simulate n grid = flashes + simulate (n - 1) grid'''
        where grid' = Data.Map.map (+ 1) grid
              dfsRoots = keys $ Data.Map.filter (== 10) grid'
              grid'' = foldl dfs grid' dfsRoots
              flashes = length $ Data.Map.filter (== 10) grid''
              grid''' = Data.Map.map reset grid''

    solve = do
        input <- (orFail . parse parseInput "Input") =<< getContents
        let grid = fromList $ concat $ zipWith mapRow [0..] input
        printf "Number of flashes is %d" (simulate 100 grid)
