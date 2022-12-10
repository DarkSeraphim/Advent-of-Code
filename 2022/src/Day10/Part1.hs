module Day10.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (endOfLine, endBy1, string, (<|>))
import Helpers.Parsec (number, Parser, parseInput)

data Inst = Nop | Addx Int

parse :: Parser [Inst]
parse = (Nop <$ string "noop" <|> Addx <$> (string "addx " *> number)) `endBy1` endOfLine

run :: Int -> [Inst] -> [Int]
run s [] = [s]
run s (Nop:rest) = s : run s rest
run s ((Addx n):rest) = s : s : run (s + n) rest

every :: Int -> [Int] -> [Int]
every n is = head is : every n (drop n is)

solve = do
  inst <- parseInput parse
  let out = zipWith (*) (run 1 inst) [1..]
  let res = sum $ take 6 $ every 40 (drop 19 out)
  printf "output: %d" res
