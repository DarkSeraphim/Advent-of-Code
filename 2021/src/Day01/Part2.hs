module Day01.Part2 (solve) where
    import Helpers.Input (readInt)
    import Text.Printf (printf)

    sum3 :: (Int, Int, Int) -> Int
    sum3 (a, b, c) = a + b + c

    solve = do
      depths <- map readInt . lines <$> getContents
      let windows = map sum3 $ zip3 depths (drop 1 depths) (drop 2 depths)

      let sol = length $ filter (uncurry (<)) $ zip windows (drop 1 windows)
      printf "We increased depth %d times" sol

