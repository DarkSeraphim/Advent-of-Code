module Day09.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec (digit, many1)
import Data.IntMap (IntMap, empty, insert, delete, elems, deleteFindMin)
import Helpers.List (cycleN)
import Debug.Trace (trace)
import Data.List (findIndex)

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

fits :: Slot -> Slot -> Bool
fits slot emptySlot = size slot <= size emptySlot

move :: [Slot] -> [Slot] -> IntMap Slot -> IntMap Slot
move [] _ filled = filled
move _ [] filled = filled
move (k:ks) emptySlots filled
   = case emptyIdx of
                  -- Leave as-is
                  Nothing -> trace (printf "%d was not found in %s" (value k) (show emptySlots)) $ move ks emptySlots filled
                  Just indexEmpty ->
                    let 
                      emptySlot = emptySlots !! indexEmpty
                      ie = index emptySlot
                      move' slot = insert ie slot $ delete (index k) filled
                      newEmpty = replaceAt emptySlots (Empty {index = ie + sk, size = size (emptySlots !! indexEmpty) - size k}) indexEmpty
                     in 
                       trace (printf "%d was found" (value k)) $ if ie >= index k
                       then move ks emptySlots filled
                       else move ks newEmpty (move' (k {index = ie}))
  where emptyIdx = findIndex (fits k) emptySlots
        replaceAt l a i = take i l ++ [a] ++ drop (i + 1) l
        sk = size k


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
  | start < k = cycleN (k - start) [0] ++ next
  | otherwise = next 
    where ((k, slot), storage') = deleteFindMin storage
          next = cycleN (size slot) [value slot] ++ toList' (k + size slot) storage'


solve :: IO ()
solve = do
  allDigits <- parseInput digits
  let (filled, empty') = process 0 0 allDigits
  let empty'' = filter (\e -> size e /= 0) empty'

  printf "Before move: %s\n\n" (toString 0 filled)
  let result = trace (show filled) $ move (reverse $ elems filled) empty'' filled
  printf "The moved string looks like this: %s\n" (toString 0 result)
  printf "And the checksum is: %d\n" (sum $ zipWith (*) (toList' 0 result) [0..])
