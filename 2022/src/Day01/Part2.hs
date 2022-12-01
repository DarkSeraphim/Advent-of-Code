module Day01.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec
import Text.Parsec (endBy1, sepBy1)
import Text.Parsec.Char (endOfLine)
import Data.List (sort)

parse = (number `endBy1` endOfLine) `sepBy1` endOfLine

solve = do
  most <- (sum . take 3) . (reverse <$> sort) . map sum <$> parseInput parse
  printf "The most three elves carry is %d" most
