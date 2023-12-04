{-# LANGUAGE TupleSections #-}
module Day04.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Data.Set (Set, fromList, intersection, size)
import Text.Parsec ( endOfLine, many1 )
import Text.Parsec.Char (char, string)
import Text.Parsec.Combinator (sepEndBy1)
import qualified Data.Map

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

buildCount :: [Int] -> Int -> Data.Map.Map Int Int
buildCount l k = Data.Map.fromList $ map (, k) l

countUp :: [Lotto] -> Data.Map.Map Int Int -> Int -> Int
countUp cards count idx 
  | idx > length cards = 0
  | otherwise = num + countUp cards count' (idx + 1)
  where num = count Data.Map.! idx
        matches = uncurry countMatches (cards !! (idx - 1))
        count' = Data.Map.unionWith (+) count (buildCount [idx + 1 .. idx + matches] num)


solve :: IO ()
solve = do
  cards <- parseInput parseCards
  let counts = buildCount [1 .. length cards] 1
  let cardCount = countUp cards counts 1
  printf "We collect %d cards\n" cardCount
