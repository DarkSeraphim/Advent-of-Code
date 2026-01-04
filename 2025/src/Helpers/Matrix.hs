module Helpers.Matrix (rowEchelonForm, augmentColumns, unaugmentColumn, showMatrix) where
import Data.List (sortOn, intercalate, findIndex, uncons, unsnoc, transpose)
import Data.Maybe (fromJust)
import Helpers.List (head')
import Debug.Trace (trace)
import Text.Printf (printf)

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

rowZero :: Integral a => [a] -> Bool
rowZero = all (==0)

gauss :: Integral a => [[a]] -> [[a]] -> [[a]]
gauss m [] = m -- We're done when we're done
gauss m (r:rest) = gauss mr rest
  where r' = positivePivot $ foldl zeroFirst r m
        mr = if rowZero r' then m else m ++ [r']

  {-
X X X 0 0
0 0 X X X
0 0 0 X X

0 S 0 D 0

    -}

-- After gauss, do column swaps to force the staircase by propagating the free variables to the right
-- We assume it's in triangle shape, so for any row r, the first nonzero index is zero in all following  rows
fixColumns :: Integral a => [[a]] -> [[a]]
fixColumns = fix 1
  where fix colIdx m' = case uncons $ drop colIdx m' of
                          Just (row, _) ->
                            case findIndex (/= 0) row of
                              Just idx -> fix (colIdx + 1) (swapColumns m' colIdx idx)
                              Nothing -> m' -- All zeroes, we're done
                          Nothing -> m' -- No rows left, we're done
        swapColumns m' src dst
          | src == dst = m'
          | otherwise = take a t ++ [t !! b] ++ drop (a + 1) (take b t) ++ [t !! a] ++ drop (b + 1) t
          where a = min src dst
                b = max src dst
                t = transpose m'

gauss' :: (Show a, Integral a) => [[a]] -> [[a]]
gauss' m
  | null m = trace "null\n" m
  | all (\r -> take 1 r == [0]) m = trace (printf "leading are zero: %s \n" (show m)) $ map ( 0 : ) (gauss' (map (drop 1) m))
  | otherwise = trace (printf "row is %s\n" (show m)) $ row : map ( 0 : ) (gauss' $ sortGE (map (drop 1 . applyRow) rows))
  where (row, rows) = fromJust $ uncons m
        applyRow r
          -- if [], zipWith has no effect anyyay
          | take 1 r == [0] = r
          | otherwise = positivePivot $ zipWith (-) r' row'
          where row' = map (* head' r) row
                r' = map (* head' row) r

sortGE :: Integral a => [[a]] -> [[a]]
sortGE = sortOn (\r -> (length $ takeWhile (==0) r, length (filter (/= 0) r)))

rowEchelonForm :: (Show a, Integral a) => [[a]] -> [[a]]
rowEchelonForm m = f sorted
  where f = trace (printf "Sorted:\n%s\n\nGEd matrix:\n%s\n\n" (showMatrix sorted) (showMatrix (gauss' sorted))) $ filter (not . rowZero) . fixColumns . gauss'

        sorted = sortGE m

-- Use Gaussian Elimination to turn our matrix into REF
rowEchelonForm' :: Integral a => [[a]] -> [[a]]
rowEchelonForm' m = filter (not . allZero) $ gauss [h] t
-- Sort by leading zeroes. Swapping is allowed for Gaussian Elimination
  where sorted = sortOn (\r -> (length $ takeWhile (==0) r, length (filter (/= 0) r))) m
        (h, t) = fromJust $ uncons sorted
        allZero :: Integral a => [a] -> Bool
        allZero = all (==0)
