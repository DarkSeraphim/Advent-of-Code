module Day04.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (number, parseInput, Parser)
import Text.Parsec (string, endOfLine, endBy1)
import Text.Parsec.String (GenParser)

type Range = (Int, Int)

pair :: Parser (Int, Int)
pair = (,) <$> (number <* string "-") <*> number
parse :: Parser [(Range, Range)]
parse = ((,) <$> pair <* string "," <*> pair) `endBy1` endOfLine 

contains' :: Range -> Range -> Bool 
contains' (amin, amax) (bmin, bmax) = 
  (amin <= bmin && bmin <= amax) 
  || 
  (amin <= bmax && bmax <= amax)
overlaps (a, b) = contains' a b || contains' b a

solve = do
  fullyContained <- length . filter overlaps <$> parseInput parse
  printf "%d ranges are overlapping " fullyContained
