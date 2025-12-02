module Day01.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Data.Functor (($>))
import Text.Parsec ( char, (<|>), endOfLine, endBy1 )
import Text.ParserCombinators.Parsec (try)

data Direction = L | R

dir :: Parser Direction
dir = try (char 'L' $> L) <|> (char 'R' $> R)

action :: Parser (Direction, Int)
action = (,) <$> dir <*> number

actions :: Parser [(Direction, Int)]
actions = action `endBy1` endOfLine

wrap :: Int -> Int
wrap i = i `mod` 100

spin :: Int -> (Direction, Int) -> Int
spin p (R, q) = p + q
spin p (L, q) = p - q

doActionCountZero :: Int -> [(Direction, Int)] -> Int
doActionCountZero _ [] = 0
doActionCountZero pos (act:remaining) = score + doActionCountZero nextPos remaining
  where nextPos = wrap (spin pos act)
        score = if nextPos == 0 then 1 else 0

solve :: IO ()
solve = do
  actions' <- parseInput actions
  let start = 50
  let res = doActionCountZero start actions'
  printf "The amount of zeroes was %d" res
