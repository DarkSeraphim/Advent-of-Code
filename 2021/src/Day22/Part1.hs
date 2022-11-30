{-# LANGUAGE TupleSections #-}
module Day22.Part1 (solve) where
    import Text.ParserCombinators.Parsec (GenParser, endBy1, string, try, (<|>))
    import Text.Parsec.Char (endOfLine)
    import Helpers.Parsec (parseInput, number)
    import Text.Printf (printf)

    type Point = (Int, Int, Int)
    type Cube = (Point, Point)
    type Step = (Bool, Cube)


    parseSteps :: GenParser Char st [Step]
    parseSteps = parseStep `endBy1` endOfLine

    parseStep :: GenParser Char st Step
    parseStep = do
        state <- parseState <* string " "
        (minX, maxX) <- string "x=" *> parseRange <* string ","
        (minY, maxY) <- string "y=" *> parseRange <* string ","
        (minZ, maxZ) <- string "z=" *> parseRange
        return (state, ((minX, minY, minZ), (maxX, maxY, maxZ)))


    parseState :: GenParser Char st Bool
    parseState = (== "on") <$> (try (string "on") <|> string "off")

    parseRange :: GenParser Char st (Int, Int)
    parseRange = (,) <$> (number <* string "..") <*> number

    isInCube :: Point -> (Point, Point) -> Bool
    isInCube (x, y, z) ((mix, miy, miz), (max, may, maz)) = xx && yy && zz
        where xx = x >= mix && x <= max
              yy = y >= miy && y <= may
              zz = z >= miz && z <= maz

    apply :: Point -> Bool -> Step -> Bool
    apply point cur (state, cube)
      | cur == state              = cur -- Regardless of the range, it's active
      | not $ isInCube point cube = cur -- Out of range
      | otherwise                 = state -- Set to the state of the step

    allPoints :: [Point]
    allPoints = concatMap (\x -> concatMap (\y -> map (x, y,) [-50..50]) [-50..50]) [-50..50]

    solve = do
        rebootSteps <- parseInput parseSteps
        let active = length $ filter (\point -> foldl (apply point) False rebootSteps) allPoints
        printf "There's %d cubes active" active

