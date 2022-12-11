module Helpers.List (chunksOf, cycleN) where
import Data.List
chunksOf :: Int -> [a] -> [[a]]
chunksOf num [] = []
chunksOf num list = take num list : chunksOf num (drop num list)
cycleN :: Int -> [a] -> [a]
cycleN 0 a = []
cycleN 1 a = a
cycleN n a
  | n < 0 = error "Negative cycle"
  | otherwise = a ++ cycleN (n - 1) a 

