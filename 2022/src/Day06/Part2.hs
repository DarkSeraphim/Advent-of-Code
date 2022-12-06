module Day06.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseInput, Parser)
import Text.Parsec ( many1, endOfLine )
import Text.Parsec.Char (noneOf)
import Data.Set (fromList)

parse :: Parser String
parse = many1 (noneOf "\n") <* endOfLine

scan :: Int -> String -> Int
scan parsed [] = error "No match"
scan parsed str
  | length (fromList check) == 14 = parsed
  | otherwise = scan (parsed + 1) rest
  where rest = tail str
        check = take 14 str

solve = do
  d <- parseInput parse
  printf "The message starts at %d" (scan 14 d)
