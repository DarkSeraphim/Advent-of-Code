module Day07.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (oneOf, count, spaces, endOfLine, sepEndBy1)
import Data.Char (ord)
import Helpers.List (frequency)
import Data.Map (elems)
import Data.List (sortBy)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Enum, Eq, Ord)

card :: Parser Char
card = oneOf "123456789TJQKA"

hand :: Parser String
hand = count 5 card

play :: Parser (String, Int)
play = (,) <$> (hand <* spaces) <*> number

plays :: Parser [(String, Int)]
plays = play `sepEndBy1` endOfLine

handType :: String -> HandType
handType hand'
  | 5 `elem` freqs = FiveOfAKind
  | 4 `elem` freqs = FourOfAKind
  | 3 `elem` freqs && 2 `elem` freqs = FullHouse
  | 3 `elem` freqs = ThreeOfAKind
  | 2 `elem` freqs && length (filter (==2) freqs) == 2 = TwoPair
  | 2 `elem` freqs = OnePair
  | otherwise = HighCard
  where freqs = elems $ frequency hand'


score :: Char -> Int
score 'A' = 14
score 'K' = 13
score 'Q' = 12
score 'J' = 11
score 'T' = 10
score  n  = ord n - ord '0'

compareCards :: String -> String -> Ordering
compareCards [] [] = EQ
compareCards (a:as) (b:bs)
  | cmp == EQ = compareCards as bs
  | otherwise = cmp
  where scoreA = score a
        scoreB = score b
        cmp = compare scoreA scoreB
compareCards _ _ = error "We should never run out of one hand alone"

compareHands :: String -> String -> Ordering
compareHands a b
  | cmp == EQ = compareCards a b
  | otherwise = cmp
  where aType = handType a
        bType = handType b
        cmp = compare aType bType

solve :: IO ()
solve = do
  plays' <- parseInput plays
  -- Sorts lowest to highest
  let sorted = sortBy (\x y -> compareHands (fst x) (fst y)) plays'
  let score' = sum $ zipWith (*) (map snd sorted) [1..]
  printf "The total score for the game was %d\n" score'
