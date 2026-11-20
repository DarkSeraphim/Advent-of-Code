module Helpers.LinearProgramming (solveMinSum) where
import Data.IntMap ( IntMap, (!), insert, fromList )
import Control.Monad (foldM)
import Helpers.List (head')
import Helpers.Matrix (rowEchelonForm, augmentColumns, unaugmentColumn)
import Data.Maybe (mapMaybe)

solveCell :: Integral a => IntMap a -> Int -> a -> a
solveCell      _   _ 0 = 0
solveCell solved idx cf = cf * (solved ! idx)

pivotIndex :: Integral a => [a] -> Int
pivotIndex = length . takeWhile (== 0)

solveRow :: Integral a => [a] -> a -> IntMap a -> Maybe a
solveRow cf b solved
  | (remainder == 0) && (res >= 0) = Just res
  | otherwise = Nothing
  where pivotIdx = pivotIndex cf
        pivotCoef = cf !! pivotIdx
        freeVars = zipWith (solveCell solved) [pivotIdx + 1 ..] (drop (pivotIdx + 1) cf)
        (res, remainder) = (b - sum freeVars) `quotRem` pivotCoef

solveCandidate :: Integral a => [[a]] -> [a] -> IntMap a -> Maybe (IntMap a)
solveCandidate mCoeffs bs solved = foldM f solved (reverse $ zip mCoeffs bs)
  where f solved' (cf, b) =  (\v -> insert pivotIdx v solved') <$> solveRow cf b solved'
          where pivotIdx = pivotIndex cf

freeCols :: Integral a => [[a]] -> [Int]
freeCols m = filter (`notElem` pivotIndices) [0 .. length (head' m) - 1]
  where pivotIndices = map pivotIndex m

  {-
    Constraints for candidate solution x: ∀xᵢ: xᵢ ≥ 0 
    -}
solveMinSum :: Integral a => a -> [[a]] -> [a] -> a
solveMinSum maxSteps coeffs bs = minimum (map sum cands)
  where ref = rowEchelonForm $ augmentColumns coeffs bs
        (coeffs', bs') = unaugmentColumn ref
        freeCols' = freeCols coeffs'
        freeColVals = map (const [0 .. maxSteps]) freeCols'
        -- Attempt: we know the sum of the candidates should definitely be less than the maxSteps
        allVals = filter (\c -> sum c <= maxSteps) $ sequence freeColVals
        candidates = map (fromList . zip freeCols') allVals
        cands = mapMaybe (solveCandidate coeffs' bs') candidates


