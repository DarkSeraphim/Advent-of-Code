module Day21.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (count, noneOf, string, (<|>), oneOf, char, endOfLine, endBy1)
import Data.Map (fromList, Map, (!), insert)
import Debug.Trace (trace)

data Operation = Plus | Minus | Mult | Div | Cmp
data Expression = Func String Operation String | Value Int

monkeyName :: Parser String
monkeyName = count 4 $ noneOf "0123456789"

toOperation :: Char -> Operation
toOperation '+' = Plus
toOperation '-' = Minus
toOperation '*' = Mult
toOperation '/' = Div
toOperation c = error $ "Invalid operation " ++ [c]

parseFunc :: Parser Expression
parseFunc = do
  lhs <- monkeyName <* char ' '
  op <- toOperation <$> oneOf "+-*/"
  rhs <- char ' ' *> monkeyName
  return $ Func lhs op rhs

parseExpression :: Parser Expression
parseExpression = parseFunc <|> (Value <$> number)

parseMonkey :: Parser (String, Expression)
parseMonkey = (,) <$> (monkeyName <* string ": ") <*> parseExpression

parseMonkeys :: Parser [(String, Expression)]
parseMonkeys = parseMonkey `endBy1` endOfLine

applyOperation :: Operation -> Int -> Int -> Int
applyOperation Plus a b = a + b
applyOperation Minus a b = a - b
applyOperation Mult a b = a * b
applyOperation Div a b = div a b
applyOperation Cmp a b = 
  case compare a b of
    LT -> -1
    EQ -> 0
    GT -> 1
solveMonkeyMath :: Map String Expression -> String -> Int
solveMonkeyMath exprs cur =
  case expr of
    Value i -> i
    Func l o r -> applyOperation o (solveMonkeyMath exprs l) (solveMonkeyMath exprs r)
  where expr = exprs ! cur

binarySearch :: (Int -> Int) -> Int -> Int -> Int
binarySearch func start search
  | cmp' < 0 = binarySearch func (start + (search `div` 2)) (search `div` 2) 
  | cmp' > 0 = binarySearch func (start - (search `div` 2)) (search `div` 2) 
  | otherwise = start
  where cmp = func start
        cmp' = trace (printf "Cmp was %d for %d with space %d" cmp start search) cmp

func monkeys' i = solveMonkeyMath monkeys'' "root"
  where monkeys'' = insert "humn" (Value i) monkeys'

solve = do
  monkeys <- fromList <$> parseInput parseMonkeys
  let (Func l o r) = monkeys ! "root"
  let monkeys' = insert "root" (Func l Cmp r) monkeys

  --let start = (2^50) `div` 2
  --printf "Start is %d" start
  --printf "The monkeys answered %d\n" (binarySearch (func monkeys') start start)
  printf "The monkeys answered %s\n" (show $ zip (map (func monkeys') [3887609741181..3887609741201]) [3887609741181..3887609741201])
