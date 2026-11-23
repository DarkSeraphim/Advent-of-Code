module Helpers.Matrix (rowEchelonForm, augmentColumns, unaugmentColumn, showMatrix) where
import Data.List (sortOn, intercalate, findIndex, uncons, unsnoc)
import Data.Maybe (fromJust)
import Helpers.List (head')

-- A matrix is defined as a list of columns

showMatrix :: Show a => [[a]] -> String
showMatrix m = intercalate "\n" $ map (unwords . map (padLeft maxLength ' ')) strM
  where strM = map (map show) m
        maxLength = maximum $ concatMap (map length) strM
        padLeft l c s = replicate (l - length s) c ++ s

augmentColumns :: [[a]] -> [a] -> [[a]]
augmentColumns = zipWith (\row value -> row ++ [value])

unaugmentColumn :: [[a]] -> ([[a]], [a])
unaugmentColumn m = unzip . fromJust $ mapM unsnoc m

positivePivot :: Integral a => [a] -> [a]
positivePivot r = case idx of
                    Nothing -> r -- all 0s
                    Just i -> map (signum (r !! i)*) r
  where idx = findIndex (/= 0) r

rowZero :: Integral a => [a] -> Bool
rowZero = all (==0)

gauss' :: Integral a => [[a]] -> [[a]]
gauss' m
  | null m = m
  | all null m = m
  | all (\r -> head' r == 0) m = map ( 0 : ) (gauss' (map (drop 1) m))
  | otherwise = row : map ( 0 : ) (gauss' $ sortGE (map (drop 1 . applyRow) rows))
  where (row, rows) = fromJust $ uncons m
        applyRow r
          -- if [], zipWith has no effect anyyay
          | head' r == 0 = r
          | otherwise = positivePivot $ zipWith (-) r' row'
          where row' = map (* head' r) row
                r' = map (* head' row) r

sortGE :: Integral a => [[a]] -> [[a]]
sortGE = sortOn (\r -> (length $ takeWhile (==0) r, length (filter (/= 0) r)))

rowEchelonForm :: Integral a => [[a]] -> [[a]]
rowEchelonForm m = filter (not . rowZero) (gauss' (sortGE m))
