{-# LANGUAGE TupleSections #-}
module Day13.Part2 (solve) where
    import Text.ParserCombinators.Parsec (endBy1, anyChar, GenParser, eof, string, char, parse)
    import Text.Parsec (endOfLine)
    import Helpers.Parsec (number)
    import Text.Printf (printf)
    import Helpers.Input (orFail)
    import Data.Set (fromList, Set, map, member)
    import Data.Foldable (minimumBy)
    import Data.List (maximumBy, intercalate)
    
    type Point = (Int, Int)
    type Fold = (Char, Int)

    parseInput :: GenParser Char st ([Point], [Fold])
    parseInput = (,) <$> points <* endOfLine <*> folds <* eof
        where points = endBy1 parsePoint endOfLine
              folds = endBy1 parseFold endOfLine

    parsePoint :: GenParser Char st Point
    parsePoint = (,) <$> number <* char ',' <*> number

    parseFold :: GenParser Char st Fold
    parseFold = (,) <$> (string "fold along " *> anyChar <* char '=') <*> number

    mirror :: Int -> Int -> Int
    mirror a b
      | a <= b = a
      | a > b  = b - (a - b)
      | otherwise = error "Unreachable"

    applyFold :: Set Point -> Fold -> Set Point
    applyFold points ('x', xMirror) = Data.Set.map (\(x, y) -> (mirror x xMirror, y)) points
    applyFold points ('y', yMirror) = Data.Set.map (\(x, y) -> (x, mirror y yMirror)) points
    applyFold _ _ = error "Unreachable"

    getX :: Point -> Int
    getX (x, y) = x
    getY :: Point -> Int
    getY (x, y) = y

    toChar :: Set Point -> Point -> Char
    toChar points point
      | point `member` points = '#'
      | otherwise             = ' '

    solve = do
        (points, folds) <- (orFail . parse parseInput "Input") =<< getContents
        let enabledSet = fromList points
        let finalSet = foldl applyFold enabledSet folds
        let minX = minimum $ Data.Set.map getX finalSet
        let minY = minimum $ Data.Set.map getY finalSet
        let maxX = maximum $ Data.Set.map getX finalSet
        let maxY = maximum $ Data.Set.map getY finalSet
        let all = Prelude.map (\y -> Prelude.map (\x -> toChar finalSet (x, y)) [minX..maxX]) [minY..maxY]

        putStrLn $ intercalate "\n" all

