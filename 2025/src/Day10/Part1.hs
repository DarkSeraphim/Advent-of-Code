module Day10.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec
import Text.Parsec hiding (space)
import Helpers.Graph (dijkstra''', computeDistance)
import Data.Bits (Bits(xor))
import Data.Set (empty, singleton)
import Data.Maybe (fromJust)


data Input = Input {output :: [Bool], toggles :: [[Int]]}

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
inputP =  Input <$> (outputP <* space) <*> togglesP <* joltageReqP

inputsP :: Parser [Input]
inputsP = inputP `endBy1` endOfLine

encodeToggle :: [Int] -> Int
encodeToggle ts = sum $ map ((2::Int)^) ts

encodeState :: [Bool] -> Int
encodeState b = sum $ map snd $ filter fst $ zip b $ map ((2 :: Int)^) [(0 :: Int)..]

solveForInput :: Input -> Int
solveForInput input = fromJust $ computeDistance end $ dijkstra''' (const 1) edges empty (singleton (0, start, start)) (==end)
  where start = 0 :: Int
        end = encodeState $ output input
        ts = map encodeToggle $ toggles input
        edges state = map (xor state) ts

solve :: IO ()
solve = do
  inputs <- parseInput inputsP
  let res = sum $ map solveForInput inputs
  printf "The sum of button presses was %d\n" res
