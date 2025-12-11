{-# LANGUAGE TupleSections #-}
module Day10.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec
import Text.Parsec hiding (space)
import Helpers.Graph (dijkstra''', computeDistance)
import Data.Bits (Bits(xor))
import Data.Set (empty, singleton)
import Data.Maybe (fromJust)
import Data.IntMap (IntMap, fromList, unionWith, filterWithKey, (!))
import Helpers.List (cycleN)


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

solveForInput :: Input -> Int
solveForInput input = fromJust $ computeDistance end $ dijkstra''' (const 1) edges empty (singleton (0, start, start)) (==end)
  where js = joltageRequirements input
        start = encodeState (replicate (length js) 0)
        end = encodeState js
        ts = map encodeToggle $ toggles input
        edges state = filter (isPossible end) $ map (unionWith (+) state) ts

solve :: IO ()
solve = do
  inputs <- parseInput inputsP
  let res = sum $ map solveForInput inputs
  printf "The sum of button presses was %d\n" res
