module Day10.Part1 (solve) where
    import Data.Map (Map, fromList, (!), member)
    import Text.Printf (printf)
    import Debug.Trace (trace)

    matchingOpen :: Map Char Char
    matchingOpen = fromList $ zip ")]}>" "([{<"

    errorCode :: Char -> Int
    errorCode ')' = 3
    errorCode ']' = 57
    errorCode '}' = 1197
    errorCode '>' = 25137
    errorCode _ = error "Not an error"

    parse :: String -> String -> Int
    parse _ [] = 0
    parse [] (next:rest)
      | member next matchingOpen = error "Wat" -- Close tag without any open tags?
      | otherwise = parse [next] rest
    parse (last:stack) (next:rest) 
      | member next matchingOpen && (matchingOpen ! next) /= last = errorCode next
      | member next matchingOpen && (matchingOpen ! next) == last = parse stack rest
      | otherwise = parse (next:last:stack) rest

    solve = do
        codes <- lines <$> getContents
        printf "Syntax sum is %d" $ sum (map (parse []) codes)

