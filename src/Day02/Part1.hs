module Day02.Part1 (solve) where
    import Data.List (sort, group, intercalate)
    import Data.Map (fromList, findWithDefault, Map)
    import Text.Printf (printf)
    import Debug.Trace (trace)

    import Data.Foldable (foldlM)

    toCounter :: String -> (Char, Int)
    toCounter s = (head s, length s)

    toCounters :: [String] -> [(Char, Int)]
    toCounters = map toCounter

    changeNum :: Int -> Char -> IO Int
    changeNum x 'U'
      | x > 3 = return $ x - 3
      | otherwise = return x
    changeNum x 'D'
      | x <= 6 = return $ x + 3
      | otherwise = return x
    changeNum x 'L'
      | mod x 3 == 1 = return x
      | otherwise = return $ x - 1
    changeNum x 'R'
      | mod x 3 == 0 = return x
      | otherwise = return $ x + 1
    changeNum _ _ = fail "Wrong instruction"


    toNumber :: Int -> String -> IO Int
    toNumber = foldlM changeNum


    toNumbers :: Int -> [String] -> IO [Int]
    toNumbers num [] = return [num]
    toNumbers num (x:xs) = do
        n <- toNumber num x
        ns <- toNumbers n xs
        return $ n : ns

    solve = do
        paths <- lines <$> getContents
        let paths' = trace ("Paths" ++ show paths) paths
        nn <- toNumbers 5 paths
        let nums = intercalate "" . map show $ nn
        printf "The input is %s" nums
