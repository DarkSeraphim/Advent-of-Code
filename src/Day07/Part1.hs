module Day07.Part1 (solve) where
    import Helpers.Parsec (number)
    import Text.ParserCombinators.Parsec.Combinator (eof, sepBy1)
    import Text.Parsec.Char (endOfLine)
    import Helpers.Input (orFail)
    import Text.ParserCombinators.Parsec (parse, char)
    import Text.Printf (printf)

    readNumbers = number `sepBy1` char ',' <* endOfLine <* eof

    computeFuel' :: [Int] -> Int -> Int
    computeFuel' nums crab = sum $ map (\x -> abs (crab - x)) nums

    computeFuel :: [Int] -> [Int]
    computeFuel nums = map (computeFuel' nums) nums

    solve = do
        nums <- (orFail . parse readNumbers "Whoops") =<< getContents
        printf "Least fuel was %d" $ minimum (computeFuel nums)
