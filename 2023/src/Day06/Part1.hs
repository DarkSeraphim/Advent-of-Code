module Day06.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (spaces, Parser, number, parseInput)
import Text.Parsec (sepEndBy1, string, endOfLine)

times :: Parser [Int]
times = string "Time:" *> spaces *> (number `sepEndBy1` spaces)

distances :: Parser [Int]
distances = string "Distance:" *> spaces *> (number `sepEndBy1` spaces)

races :: Parser [(Int, Int)]
races = zip <$> (times <* endOfLine) <*> (distances <* endOfLine)

computeDistance :: Int -> Int -> Int
computeDistance time held = held * (time - held)

countWins :: Int -> Int -> Int
countWins time distance = length $ filter (( > distance) . computeDistance time) [0..time]


solve :: IO ()
solve = do
  races' <- parseInput races
  let score = product $ map (uncurry countWins) races'
  printf "Multiplying our ways of winning, that gives %d possible outcomes\n" score 
