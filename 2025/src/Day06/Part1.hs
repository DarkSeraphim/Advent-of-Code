module Day06.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput, endBy1')
import Text.Parsec (char, many1, endOfLine, (<|>), many)
import Data.List (transpose)

data Operator = Add | Multiply

someSpaces :: Parser String
someSpaces = many (char ' ')

numberP :: Parser Int
numberP = someSpaces *> number <* someSpaces

numbersP :: Parser [Int]
numbersP = many1 numberP

allNumbersP :: Parser [[Int]]
allNumbersP = numbersP `endBy1'` endOfLine

operatorP :: Parser Operator
operatorP = someSpaces *> ((Add <$ char '+') <|> (Multiply <$ char '*')) <* someSpaces

operatorsP :: Parser [Operator]
operatorsP = many1 operatorP <* endOfLine

inputP :: Parser ([[Int]], [Operator])
inputP = (,) <$> allNumbersP <*> operatorsP

solveProblem :: Operator -> [Int] -> Int
solveProblem op = foldl1 f
  where f = case op of
              Multiply -> (*)
              Add -> (+)

solve :: IO ()
solve = do
  (numbers, operators) <- parseInput inputP
  let flipped = transpose numbers
  let res = sum $ zipWith solveProblem operators flipped
  printf "The sum of the problem solutions is %d\n" res
