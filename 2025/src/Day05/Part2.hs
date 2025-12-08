module Day05.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Range (Range, newRange, getMin, compress, size)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, endOfLine, endBy1)
import Data.List (sortOn)

freshRangeP :: Parser (Range Int)
freshRangeP = newRange <$> number <*> (char '-' *> number)

freshRangesP :: Parser [Range Int]
freshRangesP = freshRangeP `endBy1` endOfLine

ingredientsP :: Parser [Int]
ingredientsP = number `endBy1` endOfLine

inputP :: Parser ([Range Int], [Int])
inputP = (,) <$> (freshRangesP <* endOfLine) <*> ingredientsP

mergeAll :: [Range Int] -> [Range Int]
mergeAll ranges = compress sorted
  where sorted = sortOn getMin ranges

solve :: IO ()
solve = do
  (freshRanges, _) <- parseInput inputP
  let res = sum $ map size (mergeAll freshRanges)
  printf "%d ingredients are fresh" res
