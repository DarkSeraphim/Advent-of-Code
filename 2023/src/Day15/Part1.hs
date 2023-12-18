module Day15.Part1 (solve, hash) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec (noneOf, sepBy1, char, many1)
import Data.Char (ord)

pSteps :: Parser [String]
pSteps = many1 (noneOf ",\r\n") `sepBy1` char ','

update :: Int -> Char -> Int
update n c = ((n + ord c) * 17) `mod` 256

hash :: String -> Int
hash = foldl update (0 :: Int)

solve :: IO ()
solve = do
  steps <- parseInput pSteps
  let result = sum $ map hash steps
  printf "The sum of the hashes is %d\n" result
