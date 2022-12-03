module Day03.Part2 (solve) where
import Text.Printf (printf)
import qualified Data.Set
import Data.Set (intersection, toList)
import Data.Char (ord, isUpper)
import qualified Data.List

type Bag = Data.Set.Set Char

toBag = Data.Set.fromList

toScore' :: Char -> Int
toScore' c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1


toScore :: [Bag] -> Int
toScore bags = sum $ map toScore' (toList $ foldr1 intersection bags)

group :: Int -> [a] -> [[a]]
group num [] = []
group num list = Data.List.take num list : group num (Data.List.drop num list)

solve = do
  score <- group 3 . map toBag . lines <$> getContents
  printf "The bag score is %d" (sum $ map toScore score)
