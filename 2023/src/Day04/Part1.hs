module Day04.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Data.Set (Set, fromList, intersection, size)
import Text.Parsec ( endOfLine, many1 )
import Text.Parsec.Char (char, string)
import Text.Parsec.Combinator (sepEndBy1)

type Lotto = (Set Int, Set Int)

spaces :: Parser String
spaces = many1 (char ' ')

parseNumbers :: Parser (Set Int)
parseNumbers = fromList <$> number `sepEndBy1` spaces

parseCard :: Parser Lotto
parseCard = (,) <$> (string "Card" *> spaces *> number *> char ':' *> spaces *> parseNumbers <* string "|" <* spaces) <*> parseNumbers

parseCards :: Parser [Lotto]
parseCards = parseCard `sepEndBy1` endOfLine

countMatches :: Set Int -> Set Int -> Int
countMatches  a b = size $ intersection a b

solve :: IO ()
solve = do
  cards <- parseInput parseCards
  let score = sum $ map ((2 :: Int)^) $ filter (>= 0) $ map ((\x -> x - 1) . uncurry countMatches) cards
  printf "The lotto score is %d\n" score
