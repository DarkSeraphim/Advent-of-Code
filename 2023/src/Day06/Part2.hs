module Day06.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (spaces, Parser, number, parseInput)
import Text.Parsec (sepEndBy1, string, endOfLine, noneOf, many1)
import Helpers.Input (readInt)
import Text.Parsec.Char (digit)

times :: Parser String
times = concat <$> (string "Time:" *> spaces *> (many1 digit `sepEndBy1` spaces))

distances :: Parser String
distances = concat <$> (string "Distance:" *> spaces *> (many1 digit `sepEndBy1` spaces))

races :: Parser (Int, Int)
races = (,) <$> (readInt <$> times <* endOfLine) <*> (readInt <$> distances <* endOfLine)

computeDistance :: Int -> Int -> Int
computeDistance time held = held * (time - held)

countWins :: Int -> Int -> Int
countWins time distance = length $ filter (( > distance) . computeDistance time) [0..time]

solve :: IO ()
solve = do
  races' <- parseInput races
  let score = uncurry countWins races'
  printf "Multiplying our ways of winning, that gives %d possible outcomes\n" score
