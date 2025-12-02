module Day02.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Helpers.Range (Range, newRange, getMin, getMax)
import Text.Parsec (char, sepBy1)

rangeP :: Parser (Range Int)
rangeP = newRange <$> number <*> (char '-' *> number)

rangesP :: Parser [Range Int]
rangesP = rangeP `sepBy1` char ','

isValid :: Int -> Bool
isValid num = take half s /= drop half s
  where s = show num
        half = length s `div` 2

getInvalid :: Range Int -> [Int]
getInvalid r = filter (not . isValid) [getMin r .. getMax r]

solve :: IO ()
solve = do
  ranges <- parseInput rangesP
  let res = sum $ concatMap getInvalid ranges
  printf "The sum of invalid numbers is %d\n" res 
