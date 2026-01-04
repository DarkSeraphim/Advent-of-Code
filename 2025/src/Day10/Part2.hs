module Day10.Part2 (solve, bruteForce, combinationPresses) where
import Text.Printf (printf)
import Helpers.Parsec
import Text.Parsec hiding (space)
import Helpers.List (head')
import Data.List (sortOn, transpose)
import Debug.Trace (trace)
import Helpers.Matrix (augmentColumns, rowEchelonForm, unaugmentColumn, showMatrix)
import Data.IntMap.Strict (IntMap, empty)
import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.IntMap.Strict ( fromList, insert, member, union )
import qualified Data.IntMap as M
import Data.Maybe (fromJust)
import Data.List (find)

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

-- TODO: optimise this
-- Maybe order negative/positive first, so we can abort early (now we're forced to generate many impossible solutions)
combinationPresses :: Int -> S.Seq Int -> Int -> [[Int]]
combinationPresses 0 buttons _ = [replicate (S.length buttons) 0]
combinationPresses toSplit (coef S.:<| S.Empty) _ = [[toSplit `div` coef]]
combinationPresses       _              S.Empty _ = error "Can this happen?"
combinationPresses toSplit    (coef S.:<| rest) maxPress = concatMap rec cur
  where cur = reverse [0..maxPress]
        rec n = map (n :) $ combinationPresses (toSplit - (n * coef)) rest maxPress

buildButtonMatrix :: Int -> [[Int]] -> [[Int]]
buildButtonMatrix joltageCount buttons = transpose buttonCoefficients
  where buttonCoefficients = map (\b -> map (\i -> if i `elem` b then 1 else 0) [0..(joltageCount - 1)]) buttons

buildLPMatrix :: Input -> [[Int]]
buildLPMatrix input = augmentColumns buttonsMatrix joltages
  where joltages = joltageRequirements input
        buttonsMatrix = buildButtonMatrix (length joltages) (toggles input)

-- TODO: check if we have to optimise?
bruteForce :: Int -> Int -> S.Seq (Int, Int) -> [IntMap Int]
-- If it's too big, this is a likely issue
bruteForce bruteForceMultiplier sol coef = map (fromList . toList) sols
  where coef' = snd <$> coef
        presses = map S.fromList $ combinationPresses sol coef' ((bruteForceMultiplier) * maximum (fmap (abs . (sol `div`)) coef'))
        isSol :: S.Seq Int -> Bool
        isSol p = all (>= 0) p && (sol == sum (S.zipWith (*) coef' p))
        sols = map withIndex $ sortOn sum (filter isSol presses)
        withIndex = S.zipWith (\(i, _) v -> (i, v)) coef


solveRow :: Int -> [Int] -> [IntMap Int] -> Int -> [IntMap Int]
solveRow bruteForceMultiplier coefs partials const' = concatMap nFree partials
  where coefs' = S.fromList coefs
        nonZero :: S.Seq Int -> S.Seq (Int, Int)
        nonZero s = S.filter ((/= 0) . snd) $ S.mapWithIndex (,) s
        nonZeroCoefs = nonZero coefs'
        freeVariables :: IntMap Int -> S.Seq (Int, Int) -- idx, coeficient
        freeVariables partial = S.filter (not . (`member` partial) . fst) nonZeroCoefs
        freeCount :: IntMap Int -> Int
        freeCount = S.length . freeVariables
        partialSol :: IntMap Int -> Int
        partialSol partial = const' - sum (map (\(i, v) -> v * (coefs' `S.index` i)) $ M.toList partial)
        nFree :: IntMap Int -> [IntMap Int]
        nFree partial = if 1 == freeCount partial then oneFree partial else moreFree partial
        oneFree :: IntMap Int -> [IntMap Int]
        oneFree partial
          | sol >= 0 && sol * freeCoef == sol' = {-trace (printf "one for %s solving %d. sol' was %d and freeCoef was %d\n" (show partial) const' sol' freeCoef) $-} [insert freeIdx (sol' `div` freeCoef) partial]
          | sol >= 0 = {-trace (printf "solution was not fitting integer space: sol=%d sol'=%d freeCoef=%dm const'=%d" sol sol' freeCoef const') -}[]
          | otherwise = {-trace (printf "sol was negative: sol=%d, sol'=%d, freeCoef=%d, const'=%d, partial=%s" sol sol' freeCoef const' (show partial))-} []
          where sol' = partialSol partial
                sol = sol' `div` freeCoef
                (freeIdx, freeCoef) = head' $ toList $ freeVariables partial
        moreFree partial = {-trace (printf "more for %s\nnamely:\n%s\n\n" (show partial) (show newPartials)) $-} map (union partial) newPartials
          where newPartials = bruteForce bruteForceMultiplier sol' (freeVariables partial)
                sol' = partialSol partial

solveMatrixBackwards :: Int -> [[Int]] -> [Int] -> [IntMap Int]
solveMatrixBackwards _ [] [] = [empty]
solveMatrixBackwards n (coef:coefs) (const':consts) = sol
  where partial = solveMatrixBackwards n coefs consts
        sol = solveRow n coef partial const'
solveMatrixBackwards _ _ _ = error "Weird matrix"

solveForInput :: (Int, Input) -> IO Int
solveForInput (idx, input) = do
    let matrix = buildLPMatrix input
        refMatrix = rowEchelonForm matrix
        (coefs, consts) = unaugmentColumn refMatrix
        solution = fromJust $ find (not . null) $ map (\n -> solveMatrixBackwards n coefs consts) [1..10]
    printf "Matrix:\n%s\n\n" (showMatrix matrix)
    printf "Ref:\n%s\n\n" (showMatrix refMatrix)
    let res = minimum $ map sum solution
    printf "%d has result %d\n" idx res
    printf "actual presses were %s\n" (show (head' $ sortOn (sum) solution))
    return res

solve :: IO ()
solve = do
  inputs <- parseInput inputsP
  res <- sum <$> mapM solveForInput (zip [0..] inputs)
  printf "The sum of button presses was %d\n" res
