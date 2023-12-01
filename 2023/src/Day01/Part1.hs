module Day01.Part1 (solve) where
import Text.Printf (printf)
import Data.Char (isDigit)

combineNumber :: [a] -> [a]
combineNumber l = [head l, last l]

solve :: IO ()
solve = do
  lines' <- lines <$> getContents
  let lines'' = map (filter isDigit) lines'
  let numbers = map ((read :: String -> Int) . combineNumber) lines''

  printf "Calibration value is %d\n" (sum numbers)
