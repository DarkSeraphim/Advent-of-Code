module Helpers.Matrix (rowEchelonForm, augmentColumns, unaugmentColumn, showMatrix) where
import Data.List (sortOn, intercalate, findIndex, uncons, unsnoc)
import Data.Maybe (fromJust)

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

-- We know that ra and rb have the same amount of leading zeroes
-- We want rb to have less zeroes, so we LCM based on the pivot
-- and add or subtract ra to/from rb (based on the sign)
-- This leaves the leading number zero
zeroFirst :: Integral a => [a] -> [a] -> [a]
zeroFirst rb ra
  | bi == 0   = rb
  | otherwise = res
  where idx = fromJust (findIndex (/= 0) ra)
        ai = ra !! idx
        bi = rb !! idx
        ra' = map (bi*) ra
        rb' = map (ai*) rb
        res = zipWith (-) rb' ra'


positivePivot :: Integral a => [a] -> [a]
positivePivot r = case idx of
                    Nothing -> r -- all 0s
                    Just i -> map (signum (r !! i)*) r
  where idx = findIndex (/= 0) r

gauss :: Integral a => [[a]] -> [[a]] -> [[a]]
gauss m [] = m -- We're done when we're done
gauss m (r:rest) = gauss mr rest
  where r' = positivePivot $ foldl zeroFirst r m
        mr = m ++ [r']

-- Use Gaussian Elimination to turn our matrix into REF
rowEchelonForm :: Integral a => [[a]] -> [[a]]
rowEchelonForm m = filter (not . allZero) $ gauss [h] t
-- Sort by leading zeroes. Swapping is allowed for Gaussian Elimination
  where sorted = sortOn (\r -> (length $ takeWhile (==0) r, length (filter (/= 0) r))) m
        (h, t) = fromJust $ uncons sorted
        allZero :: Integral a => [a] -> Bool
        allZero = all (==0)
