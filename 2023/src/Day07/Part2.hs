module Day07.Part2 (solve, handType) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (oneOf, count, spaces, endOfLine, sepEndBy1)
import Data.Char (ord)
import Helpers.List (frequency)
import Data.Map (elems)
import Data.List (sortBy, partition)

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind | FullHouse | FourOfAKind | FiveOfAKind deriving (Enum, Eq, Ord, Show)

card :: Parser Char
card = oneOf "123456789TJQKA"

hand :: Parser String
hand = count 5 card

play :: Parser (String, Int)
play = (,) <$> (hand <* spaces) <*> number

plays :: Parser [(String, Int)]
plays = play `sepEndBy1` endOfLine

checkFrequencies :: [Int] -> (Int, Int) -> Bool
checkFrequencies freqs (a, b) = aIn && bIn && ifEqual
  where aIn = a `elem` freqs
        bIn = b `elem` freqs
        ifEqual = a /= b || (length (filter (==a) freqs) == 2)

handType :: String -> HandType
handType hand'
  | jokerCount == 5 || (5 - jokerCount) `elem` freqs = FiveOfAKind
  | jokerCount == 4 || (4 - jokerCount) `elem` freqs = FourOfAKind
  | (jokerCount == 1 && any (checkFrequencies freqs) [(2,2), (3, 1)] ) || (3 `elem` freqs && 2 `elem` freqs) = FullHouse
  | jokerCount == 3 || (3 - jokerCount) `elem` freqs = ThreeOfAKind
  | jokerCount == 2 || (jokerCount == 1 && 2 `elem` freqs) || (length (filter (==2) freqs) == 2) = TwoPair
  | jokerCount == 2 || (2 - jokerCount) `elem` freqs = OnePair
  | otherwise = HighCard
  where (jokers, notJokers) = partition (== 'J') hand'
        freqs = elems $ frequency notJokers
        jokerCount = length jokers


score :: Char -> Int
score 'A' = 14
score 'K' = 13
score 'Q' = 12
score 'J' = 1
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
