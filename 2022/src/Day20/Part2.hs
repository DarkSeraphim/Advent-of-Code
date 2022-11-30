{-# LANGUAGE TupleSections #-}
module Day20.Part2 (solve) where
    import Text.ParserCombinators.Parsec ( GenParser, endBy1, oneOf, parse )
    import Data.Map (Map, empty, fromList, keys, findWithDefault, elems)

    import Text.Parsec (endOfLine)
    import Control.Applicative (some)
    import Helpers.Input (orFail)
    import Text.Printf (printf)
    type Point = (Int, Int)

    parseInput :: GenParser Char st ([Bool], Map Point Bool)
    parseInput = (,) <$> (convertInput <$> some chr <* endOfLine) <*> (endOfLine *> parseGrid)

    isLight :: Char -> Bool
    isLight = ('#'==)

    convertInput :: String -> [Bool]
    convertInput = map isLight

    parseGrid :: GenParser Char st (Map Point Bool)
    parseGrid = do
        lines <- some chr `endBy1` endOfLine
        let x = fromList $ concat $ zipWith (\y row -> zipWith (\x value -> ((x, y), isLight value)) [0 :: Int ..] row) [0 :: Int ..] lines
        return x

    chr :: GenParser Char st Char
    chr = oneOf ".#"

    offsets = [(-1, -1), (0, -1), (1, -1),
               (-1,  0), (0,  0), (1,  0),
               (-1,  1), (0,  1), (1,  1)]
    bitValues = reverse $ map (2^) [0..8]

    addPoint :: Point -> Point -> Point
    addPoint (a, b) (c, d) = (a + c, b + d)

    find :: Map Point Bool -> Bool -> Point -> Int
    find map def point
     | value     = 1
     | otherwise = 0
       where value = findWithDefault def point map

    compute :: Bool -> [Bool] -> Map Point Bool -> Point -> (Point, Bool)
    compute def converter grid point = (point, converter !! idx)
        where points = map (addPoint point) offsets
              bits = map (find grid def) points
              idx = sum $ zipWith (*) bitValues bits

    allPointsWithin :: Int -> Int -> Int -> Int -> [Point]
    allPointsWithin minX maxX minY maxY = points
        where rangeX = [minX..maxX]
              rangeY = [minY..maxY]
              points = concatMap (\y -> map (, y) rangeX) rangeY

    simulate :: Bool -> [Bool] -> Map Point Bool -> Map Point Bool
    simulate def converter grid = fromList $ map (compute def converter grid) pointsToCheck
        where
              points = keys grid
              x = map fst points
              y = map snd points
              minX = minimum x
              maxX = maximum x
              minY = minimum y
              maxY = maximum y
              pointsToCheck = allPointsWithin (minX - 1) (maxX + 1) (minY - 1) (maxY + 1)



    runSimulation' :: Bool -> [Bool] -> Map Point Bool -> Int -> Map Point Bool
    runSimulation' _ _ grid 0 = grid
    runSimulation' def converter grid i = runSimulation' def' converter grid' (i - 1)
        where def' = if def then last converter else head converter
              grid' = simulate def converter grid

    runSimulation :: [Bool] -> Map Point Bool -> Int -> Map Point Bool
    runSimulation = runSimulation' False

    solve = do
        (converter, grid) <- (orFail . parse parseInput "Input") =<< getContents
        let result = runSimulation converter grid 50
        let lightPixels = length $ filter id $ elems result
        printf "The number of light pixels are %d\n" lightPixels
