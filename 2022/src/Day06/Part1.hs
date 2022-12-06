module Day06.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (noneOf, endOfLine, many1)
import Helpers.Parsec (Parser, parseInput)
import Data.Set (fromList)

parse :: Parser String
parse = many1 (noneOf "\n") <* endOfLine

scan :: Int -> String -> Int
scan parsed (a:b:c:d:rest)
  | length (fromList [a, b, c, d]) == 4 = parsed
  | otherwise = scan (parsed + 1) (b:c:d:rest)
scan parsed any = error "No match"

solve = do
  d <- parseInput parse
  printf "The packet starts at %d" (scan 4 d)
