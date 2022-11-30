module Day07.Part2 (solve) where
    import Helpers.Parsec (number)
    import Text.ParserCombinators.Parsec.Combinator (eof, sepBy1)
    import Text.Parsec.Char (endOfLine)
    import Helpers.Input (orFail)
    import Text.ParserCombinators.Parsec (parse, char)
    import Text.Printf (printf)
    import Debug.Trace (trace)
    
    readNumbers = number `sepBy1` char ',' <* endOfLine <* eof

    computeFuel'' :: Int -> Int -> Int
    computeFuel'' origin point =  n'
        where n = abs (point - origin)
              n' = (n * (n + 1)) `div` 2


    computeFuel' :: [Int] -> Int -> Int
    computeFuel' nums crab = sum $ map (computeFuel'' crab) nums

    computeFuel :: [Int] -> [Int]
    computeFuel nums = map (computeFuel' nums) [(minimum nums)..(maximum nums)]

    solve = do
        nums <- (orFail . parse readNumbers "Whoops") =<< getContents
        printf "Least fuel was %d\n" $ minimum (computeFuel nums)
