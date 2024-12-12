module Day11.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec ( Parser, number, spaces, parseInput )
import Text.Parsec (endOfLine, sepBy1)

stones :: Parser [Int]
stones = (number `sepBy1` spaces) <* endOfLine

numSize :: Int -> Int
numSize n = length (show n)

applyRules' :: Int -> [Int]
applyRules' 0 = [1]
applyRules' n
  | even sn = [ n `div` sep, n `mod` sep ]
  | otherwise = [2024 * n]
  where sn = numSize n
        sep = 10^(sn `div` 2)

applyRules :: [Int] -> [Int]
applyRules = concatMap applyRules'

applyN :: [Int] -> Int -> [Int]
applyN stones' 0 = stones'
applyN stones' n = applyN (applyRules stones') (n - 1)

solve :: IO ()
solve = do
  stones' <- parseInput stones
  let result = applyN stones' 25
  printf "We get %d stones after 25 blinks\n" (length result)
