module Day22.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseInput, number)
import Text.Parsec (endBy1, endOfLine)
import Data.Bits (Bits(xor))
import Data.Map (Map, fromListWith, keysSet, findWithDefault)
import Data.Set (unions, toList)

generate :: Int -> [(Int, Int)]
generate n = ((n''' `mod` 10) - (n `mod` 10) , n''' `mod` 10) : generate n'''
  where n' = mixPrune n (n * 64)
        n'' = mixPrune n' (n' `div` 32)
        n''' = mixPrune n'' (n'' * 2048)
        mixPrune x x' = (x' `xor` x) `mod` 16777216


generatePriceMap :: Int -> Map [Int] Int
generatePriceMap monkey = fromListWith (\ _ x -> x) $ map seqPrice start
  where seq' = generate monkey
        start = [0..2000 - 4]
        seqPrice :: Int -> ([Int], Int)
        seqPrice offset = (list, value)
          where nums = take 4 $ drop offset seq'
                list = map fst nums
                value = last $ map snd nums

getNumberOfBananas :: [Map [Int] Int] -> [Int] -> Int
getNumberOfBananas maps seq' = sum $ map (findWithDefault 0 seq') maps

solve :: IO ()
solve = do
  monkeyNumbers <- parseInput (number `endBy1` endOfLine)
  let maps = map generatePriceMap monkeyNumbers
  let sequences = toList $ unions $ map keysSet maps
  printf "The banana reward is %d\n" (maximum $ map (getNumberOfBananas maps) sequences)

