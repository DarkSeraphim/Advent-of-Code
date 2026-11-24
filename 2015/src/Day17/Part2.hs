module Day17.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (endOfLine)
import Helpers.Parsec (Parser, number, endBy1', parseInput)
import Data.List (sortBy)
import Data.IntMap (IntMap, singleton, empty, unionWith, findMin)

inputP :: Parser [Int]
inputP = number `endBy1'` endOfLine

fit :: Int -> Int -> [Int] -> IntMap Int
fit         0   l     _ = singleton l 1
fit         _   _    [] = empty
fit remainder len (j:js) = unionWith (+) with without
  where with = if remainder >= j then fit (remainder - j) (len + 1) js else empty
        without = fit remainder len js

solve :: IO ()
solve = do
  sizes <- sortBy (flip compare) <$>  parseInput inputP
  let res = fit 150 0 sizes
  printf "We can fit it in %d different ways\n" (snd $ findMin res)
