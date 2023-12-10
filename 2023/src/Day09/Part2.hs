module Day09.Part2 (solve, deltas, solveRow) where
import Text.Printf (printf)
import Text.Parsec (endOfLine, sepEndBy1, sepBy1, char)
import Helpers.Parsec (number, Parser, parseInput)

input :: Parser [[Int]]
input = (number `sepBy1` char ' ') `sepEndBy1` endOfLine

deltas :: [Int] -> [Int]
deltas nums = zipWith (flip (-)) nums $ tail nums

solveRow :: [Int] -> Int
solveRow row
  | all (==0) row = 0
  | otherwise = head row - solveRow (deltas row)

solve :: IO ()
solve = do
  numbers <- parseInput input
  let result = sum (map solveRow numbers)
  printf "The sum of predictions is %d\n" result
