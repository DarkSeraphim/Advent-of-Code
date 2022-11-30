module Day01.Part1 (solve) where
    import Helpers.Input (readInt)
    import Text.Printf (printf)
    solve = do
      depths <- map readInt . lines <$> getContents
      let sol = length $ filter (uncurry (<)) $ zip depths (drop 1 depths)
      printf "We increased depth %d times" sol

