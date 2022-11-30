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
    import Data.Foldable (foldlM)
    import Control.Monad (filterM)

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

    findDigit'' :: (Set Char -> IO Bool) -> [String] -> IO (Set Char)
    findDigit'' f lines = head <$> filterM f (map fromList lines)

    findByValue' :: Char -> [(Set Char, Char)] -> IO (Set Char)
    findByValue' c1 [] = fail (printf "Number %s not yet known" (show c1))
    findByValue' c1 ((cs, c2):rest)
      | c1 == c2 = return cs
      | otherwise = findByValue' c1 rest

    findByValue :: Char -> SignalDict -> IO (Set Char)
    findByValue c d = findByValue' c (toList d)

    overlap :: Set Char -> SignalDict -> Char -> IO Int
    overlap incoming dict possible = do
        known <- findByValue possible dict
        return $ length $ intersection known incoming

    findDigit :: [String] -> SignalDict -> Char -> IO (Set Char)
    findDigit signal dict '1' = findDigit'' (\s -> return $ length s == 2) signal
    findDigit signal dict '2' = findDigit'' (\s -> (length s == 5 &&) . (==2) <$> overlap s dict '4') signal
    findDigit signal dict '3' = findDigit'' (\s -> (length s == 5 &&) <$> liftA2 (&&) ((==3) <$> overlap s dict '7') ((==2) <$> overlap s dict '1')) signal
    findDigit signal dict '4' = findDigit'' (\s -> return $ length s == 4) signal
    findDigit signal dict '5' = findDigit'' (\s -> (length s == 5 &&) <$> liftA2 (&&) ((==3) <$> overlap s dict '4') ((==2) <$> overlap s dict '7')) signal
    findDigit signal dict '6' = findDigit'' (\s -> (length s == 6 &&) <$> liftA2 (&&) ((==2) <$> overlap s dict '7') ((==1) <$> overlap s dict '1')) signal
    findDigit signal dict '7' = findDigit'' (\s -> return $ length s == 3) signal
    findDigit signal dict '8' = findDigit'' (\s -> return $ length s == 7) signal
    findDigit signal dict '9' = findDigit'' (\s -> (length s == 6 &&) . (==5) <$> overlap s dict '3') signal
    findDigit signal dict '0' = findDigit'' (\s -> (length s == 6 &&) <$> liftA2 (&&) ((==4) <$> overlap s dict '3') ((==2) <$> overlap s dict '1')) signal
    findDigit signal _ char = fail (printf "Didn't find signal '%s' for char '%s'" (show signal) (show char))

    order = "1478639025"

    createDictionary' :: SignalDict -> [String] -> [Char] -> IO SignalDict
    createDictionary' dict signal [] = return dict
    createDictionary' dict signal (display:rest) = do
        signals <- findDigit signal dict display
        createDictionary' (insert signals display dict) signal rest

    createDictionary :: Line -> IO SignalDict
    createDictionary line = createDictionary' empty (signal line) order

    translateSignal :: SignalDict -> Set Char -> IO Char
    translateSignal dict signal = case res of
        Just c -> return c
        Nothing -> fail "Failed to translate"
      where res = Data.Map.lookup signal dict

    decodeLine :: Line -> IO Int
    decodeLine line = do
        dict <- createDictionary line
        displays <- mapM (translateSignal dict . fromList) (output line)
        return $ readInt displays

    solve = do
        input <- (orFail . parse parseInput "Whoops") =<< getContents
        answer <- sum <$> mapM decodeLine input
        printf "%d" answer
