module Day10.Part2 (solve) where
    import Data.Map (Map, fromList, (!), member)
    import Text.Printf (printf)
    import Debug.Trace (trace)
    import Data.List (sort)

    matchingOpen :: Map Char Char
    matchingOpen = fromList $ zip ")]}>" "([{<"

    matchingClose :: Map Char Char
    matchingClose = fromList $ zip "([{<" ")]}>"

    errorCode :: Char -> Int
    errorCode ')' = 3
    errorCode ']' = 57
    errorCode '}' = 1197
    errorCode '>' = 25137
    errorCode _ = error "Not an error"

    completionScore :: Char -> Int
    completionScore ')' = 1
    completionScore ']' = 2
    completionScore '}' = 3
    completionScore '>' = 4
    completionScore _ = error "Not a token"

    findRemaining :: String -> String -> [String]
    findRemaining [] [] = []
    findRemaining remaining [] = [map (matchingClose !) remaining]
    findRemaining [] (next:rest)
      | member next matchingOpen = error "Wat" -- Close tag without any open tags?
      | otherwise = findRemaining [next] rest
    findRemaining (last:stack) (next:rest)
      | member next matchingOpen && matchingOpen ! next /= last = []
      | member next matchingOpen && matchingOpen ! next == last = findRemaining stack rest
      | otherwise = findRemaining (next:last:stack) rest

    solve = do
        codes <- lines <$> getContents
        let incomplete = concatMap (findRemaining []) codes
        let scores = sort $ map (foldl (\score next -> score * 5 + next) 0 . map completionScore) incomplete
        printf "Remaining # is %d" $ scores !! (length scores `div` 2)
