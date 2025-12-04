module Day03.Part2 (solve) where
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

getIndex :: Int -> [Int] -> Int
getIndex x y = fromJust $ elemIndex x y

getHighestJoltageBatteries :: Int -> [Int] -> [Int]
getHighestJoltageBatteries 0 _ = []
getHighestJoltageBatteries remainingBatteries bank = next : getHighestJoltageBatteries (remainingBatteries - 1) (drop (nextIdx + 1) bank)
  where nums = nub $ sortBy (comparing Down) bank
        enough n = length bank - getIndex n bank - 1 >= (remainingBatteries - 1)
        next = maximum $ filter enough nums
        nextIdx = getIndex next bank

getHighestJoltage :: [Int] -> Int
getHighestJoltage bank = foldl1 (\acc next -> acc * 10 + next) batteries
  where batteries = getHighestJoltageBatteries 12 bank

solve :: IO ()
solve = do
  banks <- parseInput banksP
  let res = sum $ map getHighestJoltage banks
  printf "Total output joltage is %d\n" res
