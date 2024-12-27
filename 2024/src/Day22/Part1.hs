module Day22.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseInput, number)
import Text.Parsec (endBy1, endOfLine)
import Data.Bits (Bits(xor))

generate :: Int -> [Int]
generate n = n''' : generate n'''
  where n' = mixPrune n (n * 64)
        n'' = mixPrune n' (n' `div` 32)
        n''' = mixPrune n'' (n'' * 2048)
        mixPrune x x' = (x' `xor` x) `mod` 16777216

solve :: IO ()
solve = do
  monkeyNumbers <- parseInput (number `endBy1` endOfLine)
  let at2000 = map ((!! 1999) . generate) monkeyNumbers
  printf "Sum of 2000ths is %d\n" (sum at2000) 
