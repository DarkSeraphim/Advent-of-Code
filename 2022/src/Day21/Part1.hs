module Day21.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (count, noneOf, string, (<|>), oneOf, char, endOfLine, endBy1)
import Data.Map (fromList, Map, (!))

data Operation = Plus | Minus | Mult | Div
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
applyOperation Plus = (+)
applyOperation Minus = (-)
applyOperation Mult = (*)
applyOperation Div = div

solveMonkeyMath :: Map String Expression -> String -> Int
solveMonkeyMath exprs cur =
  case expr of
    Value i -> i
    Func l o r -> applyOperation o (solveMonkeyMath exprs l) (solveMonkeyMath exprs r)
  where expr = exprs ! cur

solve = do
  monkeys <- fromList <$> parseInput parseMonkeys
  printf "The monkeys answered %d\n" (solveMonkeyMath monkeys "root")
