module Day08.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (many1, oneOf, noneOf, string, endOfLine, sepEndBy1)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec.Char (char)
import Data.Map (Map, fromList, (!))

data Direction = L | R

toDirection :: Char -> Direction
toDirection 'L' = L
toDirection 'R' = R
toDirection  _  = error "Invalid direction"

directions :: Parser [Direction]
directions = map toDirection <$> many1 (oneOf "LR")

node :: Parser String
node = many1 $ noneOf " ,)\r\n"

fork :: Parser (String, (String, String))
fork = (,) <$> node <* string " = (" <*> ((,) <$> node <* string ", " <*> node <* char ')')

input :: Parser ([Direction], Map String (String, String))
input = (,) <$> directions <* endOfLine <* endOfLine <*> (fromList <$> (fork `sepEndBy1` endOfLine))

decide :: Direction -> (String, String) -> String
decide L (next, _) = next
decide R (_, next) = next

walk :: Map String (String, String) -> String -> String -> [Direction] -> Int
walk forks start end (dir:dirs)
  | start == end = 0
  | otherwise = 1 + walk forks next end dirs
  where next = decide dir (forks ! start) 
walk _ _ _ [] = error "Forgot to cycle"

solve :: IO ()
solve = do
  (directions', forks) <- parseInput input
  let len = walk forks "AAA" "ZZZ" (cycle directions')
  printf "The walk took %d directions\n" len
