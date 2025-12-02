module Day01.Part2 (solve) where
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

countZeros :: Int -> Int -> (Int, Int)
countZeros pos step = (multipleOfHundred + boundaryNum, newPos `mod` 100)
  where multipleOfHundred = abs (step `quot` 100)
        newPos = pos + (step `rem` 100)
        boundaryNum = if pos /= 0 && (newPos > 99 || newPos <= 0) then 1 else 0

delta :: (Direction, Int) -> Int
delta (L, n) = -n
delta (R, n) = n

doActionCountZero :: Int -> [(Direction, Int)] -> Int
doActionCountZero _ [] = 0
doActionCountZero pos (act:remaining) = score + doActionCountZero nextPos remaining
  where (score, nextPos) = countZeros pos (delta act)

solve :: IO ()
solve = do
  actions' <- parseInput actions
  let start = 50
  let res = doActionCountZero start actions'
  printf "The amount of zeroes was %d" res
