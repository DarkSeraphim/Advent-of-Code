module Day02.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, sepBy1', parseInput, endBy1')
import Text.Parsec (string, char, endOfLine)
import Data.Map (fromList, Map, unionsWith, elems)
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

solve :: IO ()
solve = do
  games <- parseInput parseGames
  let powers = map (product . elems . unionsWith max . map toConfig . snd) games
  let score = sum powers
  printf "Testing replace: %d\n" score
