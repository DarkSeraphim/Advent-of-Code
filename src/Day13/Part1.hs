module Day13.Part1 (solve) where
    import Text.ParserCombinators.Parsec (endBy1, anyChar, GenParser, eof, string, char, parse)
    import Text.Parsec (endOfLine)
    import Helpers.Parsec (number)
    import Text.Printf (printf)
    import Helpers.Input (orFail)
    import Data.Set (fromList, Set, map)
    
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

    solve = do
        (points, folds) <- (orFail . parse parseInput "Input") =<< getContents
        let enabledSet = fromList points
        printf "Points left after one fold: %d" (length $ applyFold enabledSet (head folds))
