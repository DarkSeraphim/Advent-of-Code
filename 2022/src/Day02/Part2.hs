module Day02.Part2 (solve) where
    import Helpers.Input (split, readInt)
    import Text.Printf (printf)
    import Debug.Trace (trace)
    add (x, y, a) ["forward", n] = (x + dx, y + dx * a, a)
        where dx = readInt n
    add (x, y, a) ["up", n] = (x, y, a - da)
        where da = readInt n
    add (x, y, a) ["down", n] = (x, y, a + da)
        where da = readInt n
    add _ _ = error "Bad input"

    solve = do
        ls <- map (split ' ') . lines <$> getContents
        let (x, y, a) = foldl add (0, 0, 0) ls
        printf "Location multiplied is %d" (x * y)
