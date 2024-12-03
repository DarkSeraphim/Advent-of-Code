module Day03.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (string, char, anyChar, (<|>), many1, try)

mul :: Parser (Int, Int)
mul = (,) <$> (string "mul(" *> number <* char ',') <*> (number <* char ')')

muls :: Parser [(Int, Int)]
muls = many1 (try mul <|> ((0, 0) <$ anyChar))

solve :: IO ()
solve = do
  muls' <- parseInput muls
  let res = sum $ map (uncurry (*)) muls'
  printf "The sum of valid products is: %d\n" res
