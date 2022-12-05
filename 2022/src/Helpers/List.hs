module Helpers.List (chunksOf) where
import Data.List
chunksOf :: Int -> [a] -> [[a]]
chunksOf num [] = []
chunksOf num list = take num list : chunksOf num (drop num list)

