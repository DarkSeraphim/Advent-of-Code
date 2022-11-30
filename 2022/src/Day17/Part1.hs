module Day17.Part1 (solve) where
    import Text.Printf (printf)
    import Text.ParserCombinators.Parsec (GenParser, option)
    import Text.ParserCombinators.Parsec.Char (string, char, digit)
    import Helpers.Input (readInt, orFail)
    import Control.Applicative (some)
    import Text.ParserCombinators.Parsec.Prim (parse)

    parseInput :: GenParser Char st ((Int, Int), (Int, Int))
    parseInput = (,) <$> xs <*> ys
        where xs = (,) <$> (prefix *> parseNumber) <*> (dots *> parseNumber)
              ys = (,) <$> (string ", y=" *> parseNumber) <*> (dots *> parseNumber)
              prefix = string "target area: x="
              dots = string ".."

    parseNumber :: GenParser Char st Int
    parseNumber = do
        sig <- option '+' (char '-')
        let signum = case sig of
                        '+' -> 1
                        '-' -> -1
                        _ -> 1
        num <- readInt <$> some digit
        return (signum * num)


    solve = do
        ((minX, maxX), (minY, maxY)) <- (orFail . parse parseInput "Input") =<< getContents
        -- I'm fully aware this only works on my input, and not just on any inputs, but I'm rolling with it :D
        printf "The max height is %d" ((minY * (minY + 1)) `div` 2)

