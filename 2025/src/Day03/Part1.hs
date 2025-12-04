module Day03.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (digit, many1, endOfLine, endBy1)
import Helpers.Parsec (Parser, parseInput)
import Helpers.Input (readIntChar)
import Data.List (sortBy, elemIndex, nub)
import Data.Ord (comparing, Down (Down))
import Data.Maybe (fromJust)

joltageP :: Parser Int
joltageP = readIntChar <$> digit

bankP :: Parser [Int]
bankP = many1 joltageP

banksP :: Parser [[Int]]
banksP = bankP `endBy1` endOfLine

getHighestNum :: [Int] -> Int -> Maybe Int
getHighestNum bank x = y
  where first = fromJust $ elemIndex x bank
        rem' = drop (first + 1) bank
        second = maximum rem'
        y = if null rem' then Nothing else Just $ (bank !! first) * 10 + second

firstJusts :: [Maybe a] -> Maybe a
firstJusts [] = Nothing
firstJusts (r@(Just _):_) = r
firstJusts (Nothing:rest) = firstJusts rest

getMaxJoltage :: [Int] -> Int
getMaxJoltage bank = fromJust $ firstJusts $ map (getHighestNum bank) nums
  where nums = nub $ sortBy (comparing Down) bank


solve :: IO ()
solve = do
  banks <- parseInput banksP
  let res = sum $ map getMaxJoltage banks
  printf "Total output joltage is %d\n" res
