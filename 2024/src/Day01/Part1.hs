module Day01.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseInput, Parser, number, spaces)
import Text.Parsec (sepEndBy1, endOfLine)
import Data.List (transpose)
import GHC.OldList (sort)

numbers :: Parser [Int]
numbers = sequence [number <* spaces, number]

diff :: [Int] -> Int
diff [a, b] = abs (a - b)
diff _ = error "Invalid input"

solve :: IO ()
solve = do
  rows <- parseInput (numbers `sepEndBy1` endOfLine)
  let res = sum $ map diff (transpose $ map sort (transpose rows))
  printf "Sum of differences is %d\n" res 
