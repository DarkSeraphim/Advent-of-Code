module Day03.Part1 (solve) where
    import Data.List (transpose, group, sort, sortOn)

    import Text.Printf (printf)
    import Data.Char (digitToInt)
    import Debug.Trace (trace)

    binDec :: String -> Int
    binDec = foldl (\acc next -> acc * 2 + digitToInt next) 0

    solve = do
        bits <- (map group <$> map sort) . transpose . lines <$> getContents
        -- Sort by length desc
        let ge = map binDec $ transpose $ map (map head . sortOn (\x -> - length x)) bits
        printf "Power consumption is %d\n" (product ge)

