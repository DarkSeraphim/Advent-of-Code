module Day08.Part2 (solve) where
    import Text.ParserCombinators.Parsec (oneOf, GenParser, endBy1, sepBy1, string, eof, parse, anyChar)
    import Control.Applicative (some, Applicative (liftA2))
    import Text.ParserCombinators.Parsec.Char (char)
    import Text.Parsec (endOfLine)
    import Helpers.Input (orFail, readInt)
    import Text.Printf (printf)
    import Data.Map ( Map, empty, insert, toList, lookup )
    import Data.Set (Set, fromList, intersection)
    import Data.Char (chr, ord)
    import Data.List (find)

    data Line = Line { signal :: [String], output :: [String]}
    type SignalDict = Map (Set Char) Char

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

    findDigit'' :: (Set Char -> Bool) -> [String] -> Set Char
    findDigit'' f lines = case display of
       Just res -> res
       Nothing -> error "No match found"
      where display = find f (map fromList lines)

    findByValue' :: Char -> [(Set Char, Char)] -> Maybe (Set Char)
    findByValue' _ [] = Nothing
    findByValue' c1 ((cs, c2):rest)
      | c1 == c2 = Just cs
      | otherwise = findByValue' c1 rest

    findByValue :: Char -> SignalDict -> Maybe (Set Char)
    findByValue c d = findByValue' c (toList d)

    overlap :: Set Char -> SignalDict -> Char -> Int
    overlap incoming dict possible = case res of
        Just known -> length $ intersection known incoming
        Nothing -> 0
      where res = findByValue possible dict

    findDigit :: [String] -> SignalDict -> Char -> Set Char
    findDigit signal dict '1' = findDigit'' (\s -> length s == 2) signal
    findDigit signal dict '2' = findDigit'' (\s -> length s == 5 && overlap s dict '4' == 2) signal
    findDigit signal dict '3' = findDigit'' (\s -> length s == 5 && overlap s dict '7' == 3 && overlap s dict '1' == 2) signal
    findDigit signal dict '4' = findDigit'' (\s -> length s == 4) signal
    findDigit signal dict '5' = findDigit'' (\s -> length s == 5 && overlap s dict '4' == 3 && overlap s dict '7' == 2) signal
    findDigit signal dict '6' = findDigit'' (\s -> length s == 6 && overlap s dict '7' == 2 && overlap s dict '1' == 1) signal
    findDigit signal dict '7' = findDigit'' (\s -> length s == 3) signal
    findDigit signal dict '8' = findDigit'' (\s -> length s == 7) signal
    findDigit signal dict '9' = findDigit'' (\s -> length s == 6 && overlap s dict '3' == 5) signal
    findDigit signal dict '0' = findDigit'' (\s -> length s == 6 && overlap s dict '3' == 4 && overlap s dict '1' == 2) signal
    findDigit signal _ char = error (printf "Didn't find signal '%s' for char '%s'" (show signal) (show char))

    order = "1478639025"

    createDictionary' :: SignalDict -> [String] -> [Char] -> SignalDict
    createDictionary' dict signal [] = dict
    createDictionary' dict signal (display:rest) = createDictionary' (insert signals' display dict) signal rest
        where signals = findDigit signal dict display
              signals' = trace (printf "Display: %s, Signals: %s" (show display) (show signals)) signals

    createDictionary :: Line -> SignalDict
    createDictionary line = createDictionary' empty (signal line) order

    translateSignal :: SignalDict -> Set Char -> Char
    translateSignal dict signal = case res of
        Just c -> c
        Nothing -> error "Failed to translate"
      where res = Data.Map.lookup signal dict

    decodeLine :: Line -> Int
    decodeLine line = readInt displays
        where dict = createDictionary line
              displays = map (translateSignal dict . fromList) (output line)

    solve = do
        input <- (orFail . parse parseInput "Whoops") =<< getContents
        printf "%d" $ sum (map decodeLine input)
