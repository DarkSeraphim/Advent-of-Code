module Day01.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseInput, number)
import Text.Parsec (sepBy1, endOfLine, many1, string, anyChar, endBy1)
import Text.Parsec.Char (char)
import Helpers.Input (split)

parse = (number `endBy1` endOfLine) `sepBy1` endOfLine

solve = do
  most <- maximum . map sum <$> parseInput parse
  printf "The most an elf carries is %d" most
