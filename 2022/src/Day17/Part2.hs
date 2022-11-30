module Day17.Part2 (solve) where
    import Text.Printf (printf)
    import Text.ParserCombinators.Parsec (GenParser, option)
    import Text.ParserCombinators.Parsec.Char (string, char, digit)
    import Helpers.Input (readInt, orFail)
    import Control.Applicative (some)
    import Text.ParserCombinators.Parsec.Prim (parse)
    import Data.Map (Map, empty, insert)

    data Bounds = Bounds { lower :: Int, upper :: Int }
    isInRange :: Int -> Bounds -> Bool
    isInRange v bounds = lower bounds <= v && v <= upper bounds

    parseInput :: GenParser Char st (Bounds, Bounds)
    parseInput = (,) <$> xs <*> ys
        where xs = Bounds <$> (prefix *> parseNumber) <*> (dots *> parseNumber)
              ys = Bounds <$> (string ", y=" *> parseNumber) <*> (dots *> parseNumber)
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

    simulate' :: Int -> Int -> Int -> Int -> (Bounds, Bounds) -> Bool
    simulate' x y xVel yVel (xBound, yBound)
      | x `isInRange` xBound && y `isInRange` yBound = True
      | xVel == 0 && not (x `isInRange` xBound) = False
      | y < lower yBound = False
      | x > upper xBound = False
      | otherwise = simulate' (x + xVel) (y + yVel) (xVel - signum xVel) (yVel - 1) (xBound, yBound)

    simulate :: (Bounds, Bounds) -> (Int, Int) -> Bool
    simulate bounds (x, y) = simulate' 0 0 x y bounds

    allCombs :: [Int] -> [Int] -> [(Int, Int)]
    allCombs x y = concatMap (\xx -> zip (repeat xx) y) x

    solve = do
        (xBound, yBound) <- orFail . parse parseInput "Input" =<< getContents
        let maxYVel = abs (lower yBound) - 1
        let minYVel = lower yBound
        let maxXVel = upper xBound
        let minXVel = 0
        printf "Y: %d - %d\n" minYVel maxYVel
        printf "X: %d - %d\n" minXVel maxXVel
        let total = length $ filter (simulate (xBound, yBound)) $ allCombs [minXVel..maxXVel] [minYVel..maxYVel]
        printf "The amount of possibilities is %d" total

