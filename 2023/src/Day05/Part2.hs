module Day05.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (string, sepBy1, char, endOfLine, sepEndBy1, count, manyTill)
import Lib (Range, newRange, includes, intersectLeft, translate, getMin, compress, overlaps, getMax)
import Text.Parsec.Char (anyChar)
import Text.ParserCombinators.Parsec (try)
import Data.Foldable (find)
import Helpers.List (chunksOf)
import Data.Maybe (mapMaybe, fromJust)
import GHC.Exts (sortWith)
import Debug.Trace (trace)

type RangeI = Range Int
data Mapper = Mapper { delta :: Int, src :: RangeI} deriving Show

seeds :: Parser [Int]
seeds = string "seeds: " *> (number `sepBy1` char ' ')

mapping :: Parser Mapper
mapping = do
  dst' <- number <* char ' '
  src' <- number <* char ' '
  len <- number
  return Mapper { delta = dst' - src', src = newRange src' (src' + len - 1) }

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
                           Just mapper -> n + delta mapper
                           Nothing -> n
  where mapperM = find (applies n) mappers


applyMappings :: [[Mapper]] -> Int -> Int
applyMappings mappers seed = foldl applyMapping seed mappers

expand :: [Int] -> RangeI
expand [a, b] = newRange a (a + b - 1)
expand _ = error "Wrong expand"

applyMapping'' :: [Mapper] -> RangeI -> [RangeI]
applyMapping'' [] r
  | getMax r < getMin r = []
  | otherwise =  [r]
applyMapping'' (m:rest) r
  | b < a = [] -- whoops we over consumed our range
  | ov && a < c = [newRange a (c - 1), translate (delta m) (fromJust (intersectLeft r mr))] ++ applyMapping'' rest (newRange (d + 1) b) -- recurse on the possible remainder of the range
  | ov = translate (delta m) (fromJust (intersectLeft r mr)) : applyMapping'' rest (newRange (d + 1) b)
  | getMax r < getMin mr = [r] -- The range is to the left of the mappings, return the range
  | getMax mr < getMin r = applyMapping'' rest r -- The mapping is to the left of r, recurse
  | otherwise = error "I missed a mapping, oops"
  where mr = src m
        ov = overlaps r mr
        a = getMin r
        b = getMax r
        c = getMin mr
        d = getMax mr


applyMapping' :: [RangeI] -> [Mapper] -> [RangeI]
applyMapping' r ms = trace ("# intervals: " ++ (show res)) res
  where res = trace (show foo) $ compress $ sortWith getMin $ concat foo
        foo = map (applyMapping'' ms) r

solve :: IO ()
solve = do
  (seeds', mappings') <- parseInput almanac
  let mappings'' = map (sortWith (getMin . src)) mappings'
  let seeds'' = map expand $ chunksOf 2 seeds'
-- let locations = map (applyMappings mappings') seeds'
  let locations = map getMin $ foldl applyMapping' seeds'' mappings''
  printf "Almanac: %d" (minimum locations)
