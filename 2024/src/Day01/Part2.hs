module Day01.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec ( Parser, parseInput, number, spaces )
import Text.Parsec (sepEndBy1, endOfLine)
import Data.Map (Map, findWithDefault)
import Helpers.List (frequency)
import Data.List (transpose)

numbers :: Parser [Int]
numbers = sequence [number <* spaces, number]

processInput :: [[Int]] -> ([Int], Map Int Int)
processInput [left, right] = (left, frequency right)
processInput _ = error "Invalid input"

freqMult :: Map Int Int -> Int -> Int
freqMult freq x = x * findWithDefault 0 x freq

solve :: IO ()
solve = do
  rows <- parseInput (numbers `sepEndBy1` endOfLine)
  let (left, freq) = processInput (transpose rows)
  let res = sum $ map (freqMult freq) left
  printf "Frequency product sum is %d\n" res
