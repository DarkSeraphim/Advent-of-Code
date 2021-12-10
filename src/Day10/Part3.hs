module Day10.Part3 (solve) where
    import Text.Printf (printf)
    import Text.ParserCombinators.Parsec (GenParser, oneOf, many, option, ParseError, parse, eof, errorPos, sourceColumn)
    import Data.Map (fromList, Map, (!))

    import Text.ParserCombinators.Parsec.Char (char)
    import Data.Functor (($>))
    import Data.List (sort)

    import Debug.Trace (trace)

    matchingClose :: Map Char Char
    matchingClose = fromList $ zip "([{<" ")]}>"

    parseEntry' :: GenParser Char st String
    parseEntry' = do
        open <- oneOf "([{<"
        let close = matchingClose ! open
        -- parse always return a singleton list
        rest <- concat <$> many parseEntry'
        case rest of
          [] -> option [close] (char close $> "")
          rest -> return (close : rest)

    parseEntry :: GenParser Char st String
    parseEntry = do
        x <- concat <$> many parseEntry'
        eof
        return $ reverse x

    parseInput :: IO [(String, Either ParseError String)]
    parseInput = map (\line -> (line, parse parseEntry "Line" line)) . lines <$> getContents

    score1 :: (String, Either ParseError a) -> Int
    score1 (line, Left err) = case c of
        ')' -> 3
        ']' -> 57
        '}' -> 1197
        '>' -> 25137
        _ -> error $ "Invalid error. Error was " ++ show err
        where c = line !! (sourceColumn (errorPos err) - 1)
    score1 _ = 0

    score2 :: (s, Either p String) -> [Int]
    -- If Right is [], then our parsing was complete. Ignore these
    score2 (_, Right []) = []
    score2 (_, Right s) = [foldl (\acc p -> acc * 5 + p) 0 $ map points s]
        where points ')' = 1
              points ']' = 2
              points '}' = 3
              points '>' = 4
              points _ = error "Bad closing character"
    score2 _ = []

    solve :: IO ()
    solve = do
        lines <- parseInput
        printf "Part 1: %d\n" (sum (map score1 lines))
        let part2 = sort $ concatMap score2 lines
        printf "Part 2: %d" (part2 !! (length part2 `div` 2))
