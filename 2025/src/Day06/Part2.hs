module Day06.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseString)
import Text.Parsec (char, many1, (<|>), many)
import Data.List ( transpose, uncons )
import Helpers.Input (readInt)
import Data.Maybe (fromJust)

data Operator = Add | Multiply deriving Show

someSpaces :: Parser String
someSpaces = many (char ' ')

operatorP :: Parser Operator
operatorP = someSpaces *> ((Add <$ char '+') <|> (Multiply <$ char '*')) <* someSpaces

operatorsP :: Parser [Operator]
operatorsP = many1 operatorP

solveProblem :: Operator -> [Int] -> Int
solveProblem op = foldl1 f
  where f = case op of
              Multiply -> (*)
              Add -> (+)

trim :: String -> String
trim s = takeWhile (/= ' ') $ dropWhile (== ' ') s

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn     _    [] = [[]]
splitOn delim (n:r)
  | n == delim = [] : next : remainder
  | otherwise  = (n : next) : remainder
  where (next, remainder) = fromJust $ uncons $ splitOn delim r
        -- Nothing is impossible, since the base case is an non-empty list. Errors indicate a bug

solve :: IO ()
solve = do
  input <- getContents
  let lines' = lines input
  let numberLines = splitOn "" $ map trim $ transpose $ init lines'
  let numbers = map (map (readInt . trim)) numberLines
  operators <- parseString operatorsP (last lines')
  let res = sum $ zipWith solveProblem operators numbers
  printf "The sum of the problem solutions is %d\n" res
