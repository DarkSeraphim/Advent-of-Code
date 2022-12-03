module Day03.Part1 (solve) where
import Text.Printf (printf)
import qualified Data.Set
import Data.Set (intersection, toList)
import Data.Char (ord, isUpper)

type Bag = Data.Set.Set Char

toBag' :: (Bag, Bag) -> String -> (Bag, Bag)
toBag' (bagA, bagB) [] = (bagA, bagB)
toBag' (bagA, bagB) (c:rest) = toBag' (bagB, Data.Set.insert c bagA) (reverse rest)

toBag = toBag' (Data.Set.empty, Data.Set.empty)

toScore' :: Char -> Int
toScore' c
  | isUpper c = ord c - ord 'A' + 27
  | otherwise = ord c - ord 'a' + 1


toScore :: (Bag, Bag) -> Int
toScore (a, b) = sum $ map toScore' (toList $ intersection a b)

solve = do
  score <- sum . map (toScore . toBag) . lines <$> getContents
  printf "The bag score is %d" score
