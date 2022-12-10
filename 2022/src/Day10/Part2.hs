module Day10.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (endOfLine, endBy1, string, (<|>))
import Helpers.Parsec (number, Parser, parseInput)
import Helpers.List (chunksOf)
import Data.List (intercalate)

data Inst = Nop | Addx Int

parse :: Parser [Inst]
parse = (Nop <$ string "noop" <|> Addx <$> (string "addx " *> number)) `endBy1` endOfLine

run :: Int -> [Inst] -> [Int]
run s [] = [s]
run s (Nop:rest) = s : run s rest
run s ((Addx n):rest) = s : s : run (s + n) rest

toChar :: Int -> Int -> Char
toChar x pos
  | pos >= x - 1 && pos <= x + 1 = '#'
  | otherwise = ' '

solve = do
  inst <- parseInput parse
  let out = zipWith toChar (run 1 inst) (cycle [0..39])
  let res = chunksOf 40 out
  printf "output:\n%s" (intercalate "\n" res)
