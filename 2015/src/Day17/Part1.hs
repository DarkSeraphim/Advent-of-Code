module Day17.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (endOfLine)
import Helpers.Parsec (Parser, number, endBy1', parseInput)
import Data.List (sortBy)

inputP :: Parser [Int]
inputP = number `endBy1'` endOfLine

fit :: Int -> [Int] -> Int
fit         0    _ = 1
fit         _   [] = 0
fit remainder (j:js) = with + without
  where with = if remainder >= j then fit (remainder - j) js else 0
        without = fit remainder js

solve :: IO ()
solve = do
  sizes <- sortBy (flip compare) <$>  parseInput inputP
  let res = fit 150 sizes
  printf "We can fit it in %d different ways\n" res
