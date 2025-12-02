module Day02.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Helpers.Range (Range, newRange, getMin, getMax)
import Text.Parsec (char, sepBy1)
import Helpers.List (chunksOf)

rangeP :: Parser (Range Int)
rangeP = newRange <$> number <*> (char '-' *> number)

rangesP :: Parser [Range Int]
rangesP = rangeP `sepBy1` char ','

isSame2 :: [String] -> Bool
isSame2 [] = False
isSame2 [_] = False
isSame2 [x, y] = x == y
isSame2 (x:y:rest) = x == y && isSame2 (y:rest)

isValid :: Int -> Bool
isValid num = not $ any (\n -> isSame2 $ chunksOf n s) $ filter (\n -> (len `rem` n) == 0) [1 .. len]
  where s = show num
        len = length s
        

getInvalid :: Range Int -> [Int]
getInvalid r = filter (not . isValid) [getMin r .. getMax r]

solve :: IO ()
solve = do
  ranges <- parseInput rangesP
  let res = sum $ concatMap getInvalid ranges
  printf "The sum of invalid numbers is %d\n" res 
