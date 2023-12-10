module Day08.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (many1, oneOf, noneOf, string, endOfLine, sepEndBy1)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec.Char (char)
import Data.Map (Map, fromList, (!), keys)

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

walk :: Map String (String, String) -> (String -> Bool) -> [Direction] -> String -> Int
walk forks end (dir:dirs) start
  | end start = 0
  | otherwise = 1 + walk forks end dirs next
  where next = decide dir (forks ! start) 
walk _ _ [] _ = error "Forgot to cycle"

solve :: IO ()
solve = do
  (directions', forks) <- parseInput input
  let lens = map (walk forks (\x -> last x == 'Z') (cycle directions')) $ filter (\x -> last x == 'A') $ keys forks
  printf "The walk took %d directions\n" (foldl1 lcm lens)
