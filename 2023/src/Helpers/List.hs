module Helpers.List (chunksOf, cycleN, combinations, combinations1, frequency) where
import Data.List
import Data.Map (Map, fromList)

chunksOf :: Int -> [a] -> [[a]]
chunksOf num [] = []
chunksOf num list = take num list : chunksOf num (drop num list)

cycleN :: Int -> [a] -> [a]
cycleN 0 a = []
cycleN 1 a = a
cycleN n a
  | n < 0 = error "Negative cycle"
  | otherwise = a ++ cycleN (n - 1) a

combinations :: [a] -> [[a]]
combinations list = [] : combinations1 list 

combinations1 :: [a] -> [[a]]
combinations1 [] = error "Empty list"
combinations1 [x] = [[x]]
combinations1 (x:rest) = map (x:) res ++ res
  where res = combinations1 rest

frequency :: Ord a => [a] -> Map a Int
frequency l = fromList $ map (\l -> (head l, length l)) . group . sort $ l
