module Day10.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec
import Text.Parsec hiding (space)
import Helpers.List (head')
import Data.List ( sortOn, transpose, find )
import Helpers.Matrix (augmentColumns, rowEchelonForm, unaugmentColumn, showMatrix)
import Data.IntMap.Strict
    ( IntMap, empty, fromList, insert, member, union )
import qualified Data.Sequence as S
import Data.Foldable (toList)
import qualified Data.IntMap as M
import Data.Maybe (fromJust)
import Helpers.LinearProgramming (solveMinSum)

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

buildButtonMatrix :: Int -> [[Int]] -> [[Int]]
buildButtonMatrix joltageCount buttons = transpose buttonCoefficients
  where buttonCoefficients = map (\b -> map (\i -> if i `elem` b then 1 else 0) [0..(joltageCount - 1)]) buttons

buildLPMatrix :: Input -> [[Int]]
buildLPMatrix input = augmentColumns buttonsMatrix joltages
  where joltages = joltageRequirements input
        buttonsMatrix = buildButtonMatrix (length joltages) (toggles input)

solveForInput :: (Int, Input) -> IO Int
solveForInput (_, input) = do
    let matrix = buildLPMatrix input
        (buttonMatrix, joltages) = unaugmentColumn matrix
        solution = solveMinSum (maximum joltages) buttonMatrix joltages
    let res = solution -- minimum $ map sum solution
    return res

solve :: IO ()
solve = do
  inputs <- parseInput inputsP
  res <- sum <$> mapM solveForInput (zip [0..] inputs)
  printf "The sum of button presses was %d\n" res
