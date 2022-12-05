module Day03.Part2 (solve) where
import Text.Printf (printf)
import qualified Data.Set
import Data.Set (intersection, toList)
import Data.Char (ord, isUpper)
import Helpers.List (chunksOf)

type Bag = Data.Set.Set Char

toBag = Data.Set.fromList

toScore' :: Char -> Int
toScore' c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1


toScore :: [Bag] -> Int
toScore bags = sum $ map toScore' (toList $ foldr1 intersection bags)

solve = do
  score <- chunksOf 3 . map toBag . lines <$> getContents
  printf "The bag score is %d" (sum $ map toScore score)
