module Day02.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, sepBy1', parseInput, endBy1')
import Text.Parsec (string, char, endOfLine)
import Data.Map (fromList, Map, toList, (!))
import Text.Parsec.Char (noneOf)
import Control.Applicative (Alternative(some))

type Cube = (Int, String)
type Draw = [Cube]
type Game = (Int, [Draw])

colon :: Parser String
colon = string ": "

comma :: Parser String
comma = string ", "

semi :: Parser String
semi = string "; "

parseCube :: Parser Cube
parseCube = (,) <$> (number <* char ' ') <*> some (noneOf ",;\r\n")

parseDraw :: Parser Draw
parseDraw = sepBy1' parseCube comma

parseGame :: Parser Game
parseGame = (,) <$> (string "Game " *> number <* colon) <*> sepBy1' parseDraw semi

parseGames :: Parser [Game]
parseGames = endBy1' parseGame endOfLine

toConfig :: Draw -> Map String Int
toConfig d = fromList (map (\(i, j) -> (j, i)) d)

isValidConfiguration :: Draw -> Draw -> Bool
isValidConfiguration config game = foldl (\acc (s, n) -> acc && (c ! s) >= n) True d
  where c = toConfig config
        d = toList (toConfig game)

solve :: IO ()
solve = do
  games <- parseInput parseGames
  let config = [(12, "red"), (13, "green"), (14, "blue")]
  let validGames = filter (\(_, draws) -> foldl (\acc draw -> acc && isValidConfiguration config draw) True draws) games
  let score = sum $ map fst validGames
  
  printf "The sum was: %d\n" score
