module Day03.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (number, StatefulParser, parseInputWithState)
import Text.Parsec (string, char, many1, try, (<|>), anyChar, setState, getState)
import Data.Functor (($>))

dont :: StatefulParser Bool (Int, Int)
dont = string "don't()" *> setState False $> (0, 0)

do' :: StatefulParser Bool (Int, Int)
do' = string "do()" *> setState True $> (0, 0)

mul :: StatefulParser Bool (Int, Int)
mul = (,) <$> (string "mul(" *> number <* char ',') <*> (number <* char ')')

nop :: StatefulParser Bool (Int, Int)
nop = fail "nop"

mul' :: StatefulParser Bool (Int, Int)
mul' = do
  active <- getState
  try dont <|> try do' <|> try (if active then mul else nop) <|> ((0, 0) <$ anyChar)

muls :: StatefulParser Bool [(Int, Int)]
muls = many1 mul'


solve :: IO ()
solve = do
  muls' <- parseInputWithState muls True
  let res = sum $ map (uncurry (*)) muls'
  printf "The sum of valid products (with conditionals) is: %d\n" res
