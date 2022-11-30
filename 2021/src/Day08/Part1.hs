module Day08.Part1 (solve) where
    import Text.ParserCombinators.Parsec (oneOf, GenParser, endBy1, sepBy1, string, eof, parse, anyChar)
    import Control.Applicative (some, Applicative (liftA2))
    import Text.ParserCombinators.Parsec.Char (char)
    import Text.Parsec (endOfLine)
    import Helpers.Input (orFail)
    import Text.Printf (printf)
    import Debug.Trace (trace)

    data Line = Line { signal :: [String], output :: [String]}

    parseInput :: GenParser Char st [Line]
    parseInput = endBy1 parseLine endOfLine <* eof

    parseLine :: GenParser Char st Line
    parseLine = liftA2 Line (uniqueSignal <* string "| ") outputSignal
    
    uniqueSignal :: GenParser Char st [String]
    uniqueSignal = endBy1 display $ char ' '

    outputSignal :: GenParser Char st [String]
    outputSignal = sepBy1 display $ char ' '

    display :: GenParser Char st String
    display = some $ oneOf "abcdefg"

    countDigit' :: (String -> Bool) -> [Line] -> Int
    countDigit' f lines = length $ filter f $ concatMap output lines

    countDigit :: [Line] -> Int -> Int
    countDigit lines 1 = countDigit' (\s -> length s == 2) lines
    countDigit lines 4 = countDigit' (\s -> length s == 4) lines
    countDigit lines 7 = countDigit' (\s -> length s == 3) lines
    countDigit lines 8 = countDigit' (\s -> length s == 7) lines
    countDigit lines _ = 0

    solve = do
        input <- (orFail . parse parseInput "Whoops") =<< getContents
        printf "%d" $ sum (map (countDigit input) [0..9])
