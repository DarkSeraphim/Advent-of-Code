module Day05.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Range (Range, newRange, includes)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, endOfLine, endBy1)

freshRangeP :: Parser (Range Int)
freshRangeP = newRange <$> number <*> (char '-' *> number)

freshRangesP :: Parser [Range Int]
freshRangesP = freshRangeP `endBy1` endOfLine

ingredientsP :: Parser [Int]
ingredientsP = number `endBy1` endOfLine

inputP :: Parser ([Range Int], [Int])
inputP = (,) <$> (freshRangesP <* endOfLine) <*> ingredientsP

isFresh :: [Range Int] -> Int -> Bool
isFresh fresh ingredient = any (includes ingredient) fresh

solve :: IO ()
solve = do
  (freshRanges, ingredients) <- parseInput inputP
  let res = length $ filter (isFresh freshRanges) ingredients
  printf "%d ingredients are fresh" res
