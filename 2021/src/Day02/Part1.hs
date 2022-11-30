module Day02.Part1 (solve) where
    import Helpers.Input (split, readInt)
    import Text.Printf (printf)
    add (x, y) ["forward", n] = (x + dx, y)
        where dx = readInt n
    add (x, y) ["up", n] = (x, y - dy)
        where dy = readInt n
    add (x, y) ["down", n] = (x, y + dy)
        where dy = readInt n
    add _ _ = error "Bad input"

    solve = do
        ls <- map (split ' ') . lines <$> getContents
        let (x, y) = foldl add (0, 0) ls
        printf "Location multiplied is %d" (x * y)
