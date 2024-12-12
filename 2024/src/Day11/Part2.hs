module Day11.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec ( parseInput, Parser, number, spaces )
import Text.Parsec (endOfLine, sepBy1)
import Data.Map (Map, (!?), insert, empty)
 
type CacheKey = (Int, Int)
type Cache = Map CacheKey Int
 
stones :: Parser [Int]
stones = (number `sepBy1` spaces) <* endOfLine
 
numSize :: Int -> Int
numSize = length . show
 
applyRule :: Int -> [Int]
applyRule 0 = [1]
applyRule n
  | even sn = [n `div` pow, n `mod` pow]
  | otherwise = [2024 * n]
  where sn = numSize n
        pow = 10^(sn `div` 2)
 
 
computeStones :: Cache -> Int -> Int -> (Cache, Int)
computeStones cache     0   _ = (cache, 1)
computeStones cache depth num = case hit of
                        Just value -> (cache, value)
                        Nothing -> (newCache', result)
  where hit = cache !? (num, depth)
        nextNumbers = applyRule num
        (newCache, result) = foldl (computeStones' depth) (cache, 0) nextNumbers
        newCache' = insert (num, depth) result newCache
 
computeStones' :: Int -> (Cache, Int) -> Int -> (Cache, Int)
computeStones' depth (cache', acc) num' = (cache'', acc + int)
  where (cache'', int) = computeStones cache' (depth - 1) num'
 
foos :: Cache -> Int -> [Int] -> (Cache, Int)
foos cache depth = foldl (computeStones' depth) (cache, 0)
 
solve :: IO ()
solve = do
  stones' <- parseInput stones
  let (_, res) =  foos empty 76 stones'
  printf "After 25 iterations, we have %d stones\n" res
