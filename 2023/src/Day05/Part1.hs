module Day05.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (string, sepBy1, char, endOfLine, sepEndBy1, count, manyTill)
import Lib (Range, newRange, includes, getMin)
import Text.Parsec.Char (anyChar)
import Text.ParserCombinators.Parsec (try)
import Data.Foldable (find)

data Mapper = Mapper { dst :: Int, src :: Range Int} deriving Show

seeds :: Parser [Int]
seeds = string "seeds: " *> (number `sepBy1` char ' ')

mapping :: Parser Mapper
mapping = do
  dst' <- number <* char ' '
  src' <- number <* char ' '
  len <- number
  return Mapper { dst = dst', src = newRange src' (src' + len - 1) }

mappings :: Parser [Mapper]
mappings = mapping `sepEndBy1` endOfLine

almanac :: Parser ([Int], [[Mapper]])
almanac = do
  seeds' <- seeds <* count 2 endOfLine
  mappings' <- (manyTill anyChar (try endOfLine) *> mappings) `sepEndBy1` endOfLine
  return (seeds', mappings')

applies :: Int -> Mapper -> Bool
applies i m = includes i (src m)

applyMapping :: Int -> [Mapper] -> Int
applyMapping n mappers = case mapperM of
                           Just mapper -> n + dst mapper - getMin (src mapper)
                           Nothing -> n
  where mapperM = find (applies n) mappers
  

applyMappings :: [[Mapper]] -> Int -> Int
applyMappings mappers seed = foldl applyMapping seed mappers


solve :: IO ()
solve = do
  (seeds', mappings') <- parseInput almanac
  let locations = map (applyMappings mappings') seeds'
  printf "Almanac: %d" (minimum locations)
