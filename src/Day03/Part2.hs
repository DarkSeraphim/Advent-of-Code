module Day03.Part2 (solve) where
    import Data.List (transpose, group, sort, sortOn)

    import Text.Printf (printf)
    import Data.Char (digitToInt, ord)
    import Debug.Trace (trace)

    binDec :: String -> Int
    binDec = foldl (\acc next -> acc * 2 + digitToInt next) 0

    findNum' :: Ord a => ([Char] -> a) -> [String] -> String
    findNum' s [x] = x
    findNum' s x = common : findNum' s (map tail (filter ((== common) . head) x))
        where common = head . head $ sortOn s $ group $ sort $ map head x

    findOxygen = findNum' (\c -> (- length c,  - ord (head c)))
    findCO2 = findNum' (\c -> (length c, ord (head c)))

    solve = do
        bits <- lines <$> getContents
        -- Sort by length desc
        let ge = map binDec [findOxygen bits, findCO2 bits]
        printf "Power consumption is %d\n" (product ge)

