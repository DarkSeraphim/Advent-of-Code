module Day07.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, spaces, parseInput, numberInteger)
import Text.Parsec (sepBy1, string, endOfLine, endBy1)

type Formula = (Integer, [Integer])

formula :: Parser Formula
formula = (,) <$> numberInteger <* string ": " <*> (numberInteger `sepBy1` spaces)

formulas :: Parser [Formula]
formulas = formula `endBy1` endOfLine

concat' :: Integer -> Integer -> Integer
concat' a b = read $ show a ++ show b

resolve :: Integer -> [Integer] -> [Integer]
resolve cur [] = [cur]
resolve cur (o:os) = plus ++ mult ++ conc
  where resolve' next = resolve next os
        plus = resolve' (cur + o)
        mult = resolve' (cur * o)
        conc = resolve' (concat' cur o)


validate :: Formula -> Bool
validate (result, operands) = result `elem` results
  where results = resolve (head operands) (tail operands)

solve :: IO ()
solve = do
  formulas' <- parseInput formulas
  let valid = filter validate formulas'
  let total = sum $ map fst valid
  printf "The sum of valid solutions is %d\n" total
