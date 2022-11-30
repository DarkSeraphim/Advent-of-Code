{-# LANGUAGE TupleSections #-}
module Day22.Part2 (solve) where
    import Text.ParserCombinators.Parsec (GenParser, endBy1, string, try, (<|>))
    import Text.Parsec.Char (endOfLine)
    import Helpers.Parsec (parseInput, number)
    import Text.Printf (printf)
    import Debug.Trace (trace)
    import Data.Maybe (isJust, catMaybes, mapMaybe)
    import Data.Map (Map, singleton, toList, fromList, empty, unionsWith, filter)

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

    getSize ((a, b, c), (d, e, f)) = (1 + abs (d - a)) * (1 + abs (e - b)) * (1 + abs (f - c))

    min' :: Point -> Point -> Point
    min' (a, b, c) (d, e, f) = (min a d, min b e, min c f)

    max' :: Point -> Point -> Point
    max' (a, b, c) (d, e, f) = (max a d, max b e, max c f)

    isValidCuboid (a, b, c) (d, e, f) = a <= d && b <= e && c <= f

    getOverlap :: Cube -> Cube -> Maybe Cube
    getOverlap a@(aMin, aMax) b@(bMin, bMax)
      -- We 
        | isValidCuboid minC maxC = Just (minC, maxC)
        | otherwise               = Nothing
          where (minC, maxC) = (max' aMin bMin, min' aMax bMax)

    processStep'' :: Cube -> (Cube, Int) -> Maybe (Cube, Int)
    processStep'' cube (c, i) = (,-i) <$> getOverlap cube c

    processStep' :: [Step] -> Map Cube Int -> Map Cube Int
    processStep' [] values = values
    processStep' ((state, cube):steps) values = processStep' steps values'
        where negate = unionsWith (+) $ map (uncurry singleton) $ mapMaybe (processStep'' cube) (toList values)
              add = if state then singleton cube 1 else empty
              values' = unionsWith (+) [add, negate, values]

    getResult :: (Cube, Int) -> Int
    getResult (cube, num) = getSize cube * num

    solve = do
        rebootSteps <- parseInput parseSteps
        let result = processStep' rebootSteps empty
        let value = sum $ map getResult $ toList result
        printf "There's %d cubes active\n" value


