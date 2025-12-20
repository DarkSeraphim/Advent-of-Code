{-# LANGUAGE TupleSections #-}
module Day10.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec
import Text.Parsec hiding (space)
import Helpers.Graph (dijkstra''', computeDistance)
import Data.Bits (Bits(xor))
import Data.Set (empty, singleton)
import Data.Maybe (fromJust)
import Data.IntMap.Strict (IntMap, fromList, unionWith, filterWithKey, (!), unionsWith, insertWith, keys)
import Helpers.List (cycleN, head', tail')
import Data.List (sortOn, sort, nub, maximumBy, transpose)
import Control.Applicative (asum)
import Debug.Trace (trace)
import qualified Data.IntMap as M
import Data.List (partition)
import Helpers.Matrix (augmentColumns, rowEchelonForm, unaugmentColumn, showMatrix)
import Data.Fixed (mod')
import qualified Data.Set as S


data Input = Input {output :: [Bool], toggles :: [[Int]], joltageRequirements :: [Int]}

space :: Parser Char
space = char ' '

outputP :: Parser [Bool]
outputP = map (=='#') <$> (char '[' *> many1 (oneOf ".#") <* char ']')

toggleP :: Parser [Int]
toggleP = char '(' *> (number `sepBy1` char ',') <* char ')'

togglesP :: Parser [[Int]]
togglesP = toggleP `endBy1'` space

joltageReqP :: Parser [Int]
joltageReqP = char '{' *> (number `sepBy1` char ',') <* char '}'

inputP :: Parser Input
inputP =  Input <$> (outputP <* space) <*> togglesP <*> joltageReqP

inputsP :: Parser [Input]
inputsP = inputP `endBy1` endOfLine

encodeToggle :: [Int] -> IntMap Int
encodeToggle ts = fromList $ map (,1) ts

encodeState :: [Int] -> IntMap Int
encodeState js = fromList $ zip [0..] js

isPossible :: IntMap Int -> IntMap Int -> Bool
isPossible end state = null $ filterWithKey (\k v -> (end ! k) < v) state

sortButtonOnImpact :: [[Int]] -> IntMap Int -> [[Int]]
sortButtonOnImpact buttons joltages = sortOn f buttons
  where outputCount = unionsWith (+) $ map encodeToggle buttons
        f a = (minimum (map (outputCount !) a), sort $ map (\b -> -(joltages ! b)) a)

canPress :: IntMap Int -> [[Int]] -> [[Int]]
canPress state = filter f
  where f = all ((>0) . (state !))

canComplete :: IntMap Int -> [[Int]] -> Bool
canComplete delta buttons = all (`elem` allButtons) $ keys (M.filter (/=0) delta)
  where allButtons = nub $ sort $ concat buttons

applyPresses :: IntMap Int -> [Int] -> Int -> IntMap Int
applyPresses state buttons n = foldl f state buttons
  where f state' b' = insertWith (+) b' n state'

minRequiredNotPossible :: IntMap Int -> [[Int]] -> Bool
minRequiredNotPossible delta remainingButtons = or (M.mapWithKey f delta)
  -- f checks if there is a key, for which the value required is more than the available presses
  -- we can probably prune more, because this doesn't consider the side effects between buttons
  where f k v = v > sum (map (maxPresses delta) $ filter (k `elem`) remainingButtons)

maxPresses :: IntMap Int -> [Int] -> Int
maxPresses delta button = minimum $ map (delta !) button

combinationPresses :: Int -> Int -> [[Int]]
combinationPresses       0 buttons = [replicate buttons 0]
combinationPresses toSplit       1 = [[toSplit]]
combinationPresses       _       0 = error "Can this happen?"
combinationPresses toSplit buttons = concatMap rec cur
  where cur = reverse [0..toSplit]
        rec n = map (n :) $ combinationPresses (toSplit - n) (buttons - 1)

dfs2 :: [[Int]] -> IntMap Int -> IntMap Int -> Maybe Int
dfs2 buttons goal state
  | goal == state = Just 0
  | null delta = Nothing
  | any (<0) delta = Nothing
  | null applicableButtons = Nothing
  | otherwise = (joltageValue + ) <$> asum (map (dfs2 restButtons goal . applyCombination applicableButtons state) combinations)
  where delta = M.filter (>0) $ unionWith (-) goal state
        outputCount = unionsWith (+) $ map encodeToggle buttons
        (joltageIdx, joltageValue) = maximum $ sortOn (\(k, v) -> (outputCount ! k, -v)) (M.toList delta)
        (applicableButtons, restButtons) =  partition (joltageIdx `elem`) buttons
        combinations = combinationPresses joltageValue (length applicableButtons)
        applyCombination :: [[Int]] -> IntMap Int -> [Int] -> IntMap Int
        applyCombination buttons' state' comb = state''
          where state'' =  foldl (\s (n, b) -> applyPresses s b n) state' $ filter ((/= 0) . fst) (zip comb buttons')


dfs :: [[Int]] -> IntMap Int -> IntMap Int -> Maybe Int
dfs buttons goal state
-- we're done
  | goal == state = Just 0
-- we can achieve the sum of the buttons
  | minRequiredNotPossible diff buttonsLeft = Nothing
-- everything we need to be able to press is remaining
  | not $ canComplete diff buttonsLeft = Nothing
-- nothing left to press, but not done
  | null buttonsLeft = Nothing
  | otherwise = asum $ map rec (reverse [0..maxPresses'])
  where diff = unionWith (-) goal state
        buttonsLeft = canPress diff buttons
        pressing = head' buttonsLeft
        maxPresses' = maxPresses diff pressing
        rec n = (n +) <$> dfs (drop 1 buttonsLeft) goal (applyPresses state pressing n)

buildButtonMatrix :: Int -> [[Int]] -> [[Int]]
buildButtonMatrix joltageCount buttons = transpose buttonCoefficients
  where buttonCoefficients = map (\b -> map (\i -> if i `elem` b then 1 else 0) [0..(joltageCount - 1)]) buttons

buildLPMatrix :: Input -> [[Int]]
buildLPMatrix input = augmentColumns buttonsMatrix joltages
  where joltages = joltageRequirements input
        buttonsMatrix = buildButtonMatrix (length joltages) (toggles input)

bruteForce :: Int -> [Int] -> [[Int]]
-- If it's too big, this is a likely issue
bruteForce sol coef = takeWhile ((minSum ==) . sum)  sols
  where presses = combinationPresses sol (length coef)
        isSol p = sol == sum (zipWith (*) coef p)
        sols = sortOn sum (filter isSol presses)
        minSum = sum $ head' sols


solveRow :: [Int] -> [[Int]] -> Int -> [[Int]]
solveRow coefs partials const' = concatMap nFree partials
  where nonZero = dropWhile (==0) coefs
        freeCount partial = length nonZero - length partial
        partialSol partial = const' - sum (zipWith (*) (reverse partial) (reverse coefs))
        nFree partial = if 1 == freeCount partial then [oneFree partial] else moreFree partial
        oneFree partial = trace (printf "one free for partial %s\n" (show partial)) $ partialSol partial `div` head' nonZero : partial
        moreFree partial = trace (printf "more free for partial %s\n" (show partial)) map ( ++ partial) $ bruteForce (partialSol partial) (take (freeCount partial) nonZero)

solveMatrixBackwards :: [[Int]] -> [Int] -> [[Int]]
solveMatrixBackwards [] [] = [[]]
solveMatrixBackwards (coef:coefs) (const':consts) = trace (printf "Solution: %s\n" (show sol)) sol 
  where partial = solveMatrixBackwards coefs consts
        sol = solveRow coef partial const'

solveMatrixBackwards _ _ = error "Weird matrix"

dedupeMatrix :: [[Int]] -> [[Int]]
dedupeMatrix m = dedupe m S.empty
  where dedupe [] _ = []
        dedupe (r:rest) seen
          | r `S.member` seen = dedupe rest seen
          | otherwise = r : dedupe rest (S.insert r seen)

solveForInput :: (Int, Input) -> IO Int
solveForInput (idx, input) = do
    let matrix = buildLPMatrix input
        refMatrix = rowEchelonForm (dedupeMatrix matrix)
        (coefs, consts) = unaugmentColumn refMatrix
        solution = solveMatrixBackwards coefs consts
    printf "Matrix:\n%s\n\n" (showMatrix matrix)
    printf "Deduped:\n%s\n\n" (showMatrix $ dedupeMatrix matrix)
    let res = minimum $ map sum solution
    printf "%d has result %d\n" idx res
    return res
    where js = encodeState $ joltageRequirements input
          ts = sortButtonOnImpact (toggles input) js
          start = fromList $ map (,0) $ keys js

solve :: IO ()
solve = do
  inputs <- parseInput inputsP
  res <- sum <$> mapM solveForInput (zip [0..] inputs)
  printf "The sum of button presses was %d\n" res
