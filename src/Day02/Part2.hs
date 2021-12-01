module Day02.Part2 (solve) where
    import Data.List (sort, group, intercalate)
    import Data.Map (fromList, findWithDefault, Map, lookup, (!))
    import Text.Printf (printf)
    import Debug.Trace (trace)

    import Data.Foldable (foldlM)

    u c = ('U', c)
    r c = ('R', c)
    d c = ('D', c)
    l c = ('L', c)

    graph :: Map Char (Map Char Char)
    graph = fromList 
        [
            ('1', fromList [d '3']),
            ('2', fromList [r '3', d '6']),
            ('3', fromList [u '1', r '4', d '7', l '2']),
            ('4', fromList [d '8', l '3']),
            ('5', fromList [r '6']),
            ('6', fromList [u '2', r '7', d 'A', l '5']),
            ('7', fromList [u '3', r '8', d 'B', l '6']),
            ('8', fromList [u '4', r '9', d 'C', l '7']),
            ('9', fromList [l '8']),
            ('A', fromList [u '6', r 'B']),
            ('B', fromList [u '7', r 'C', d 'D', l 'A']),
            ('C', fromList [u '8', l 'B']),
            ('D', fromList [u 'B'])
        ]
    
    walk :: Char -> String -> Char
    walk current [] = current
    walk current (next:rest) = walk node rest
        where node = findWithDefault current next $ (!) graph current

    solve' :: Char -> [String] -> String
    solve' current [] = []
    solve' current (key:rest) = c : solve' c rest
        where c = walk current key

    solve = do
        paths <- lines <$> getContents
        let paths' = trace ("Paths" ++ show paths) paths
        let nums = solve' '5' paths'
        printf "The input is %s" nums
