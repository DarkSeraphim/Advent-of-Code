{-# OPTIONS_GHC -Wno-partial-fields #-}
module Day09.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec (digit, many1)
import Data.IntMap (IntMap, empty, insert, delete, elems, deleteFindMin)
import Helpers.List (cycleN)
import Debug.Trace (trace)

data Slot = Slot { value :: Int, index :: Int, size :: Int } | Empty { index :: Int, size :: Int} deriving Show

digit' :: Parser Int
digit' = read <$> sequence [digit]

digits :: Parser [Int]
digits = many1 digit'

process :: Int -> Int -> [Int] -> (IntMap Slot, [Slot])
process   _    _ [] = (empty, [])
process idx id' (n:ns) = (reg', emp')
  where (reg, emp) = process (idx + n) (id' + 1) ns
        free = odd id'
        reg' = if free then reg else insert idx (Slot {value = id' `div` 2, index = idx, size = n}) reg
        emp' = if free then Empty {index = idx, size = n} : emp else emp

move :: [Slot] -> [Slot] -> IntMap Slot -> IntMap Slot
move [] _ filled = filled
move _ [] filled = filled
move (k:ks) (e:es) filled
  | se == 0 = move (k:ks) es filled
  | ie >= index k = filled
  | sk < se = move ks (Empty {index = ie + sk, size = se - used} : es) filled''
  | sk > se = move (k {size = sk - used} : ks) es filled'''
  | otherwise = move ks es filled''
  where sk = size k
        se = size e
        used = min sk se
        ie = index e
        filled' = insert ie (Slot {value = value k, index = ie, size = used}) filled
        filled'' = delete (index k) filled'
        filled''' = insert (index k) (k {size = sk - used}) filled''


toString :: Int -> IntMap Slot -> String
toString start storage
  | null storage = ""
  | start < k = cycleN (k - start) "." ++ next
  | otherwise = next
  where ((k, slot), storage') = deleteFindMin storage
        next = cycleN (size slot) (show $ value slot `mod` 10) ++ toString (k + size slot) storage'

toList' :: Int -> IntMap Slot -> [Int] 
toList' start storage
  | null storage = []
  | start < k = error "Gap?"
  | otherwise = cycleN (size slot) [value slot] ++ toList' (k + size slot) storage'
  where ((k, slot), storage') = deleteFindMin storage

solve :: IO ()
solve = do
  allDigits <- parseInput digits
  let (filled, empty') = process 0 0 allDigits

  printf "Before move: %s\n\n" (toString 0 filled)
  let result = trace (show filled) $ move (reverse $ elems filled) empty' filled
  printf "The moved string looks like this: %s\n" (toString 0 result)
  printf "And the checksum is: %d\n" (sum $ zipWith (*) (toList' 0 result) [0..])
