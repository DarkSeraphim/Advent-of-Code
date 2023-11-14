module Day25.Part1 (solve) where
import Text.Printf (printf)
import GHC.Float (float2Int, int2Float)
import Debug.Trace (trace)

newtype Snafu = Snafu Int deriving (Eq, Ord)

instance Num Snafu where
  (+) (Snafu i) (Snafu j) = Snafu (i + j)
  (*) (Snafu i) (Snafu j) = Snafu (i * j)
  abs (Snafu i) = Snafu (abs i)
  signum (Snafu i) = Snafu (signum i)
  fromInteger i = Snafu (fromInteger i)
  negate (Snafu i) = Snafu (negate i)

charToNum :: Char -> Int
charToNum '2' = 2
charToNum '1' = 1
charToNum '0' = 0
charToNum '-' = -1
charToNum '=' = -2
charToNum _ = error "Invalid SNAFU character"

numToChar :: Int -> Char
numToChar 2 = '2'
numToChar 1 = '1'
numToChar 0 = '0'
numToChar (-1) = '-'
numToChar (-2) = '='
numToChar _ = error "Invalid SNAFU digit"

fromSnafu :: String -> Int
fromSnafu s =sum $ zipWith (\p c -> p * charToNum c) (map (5^) [0..]) (reverse s)

numChars :: Int -> Int
numChars n = if n <= bit then exp - 1 else exp
  where exp = float2Int (logBase 5 (int2Float n)) + 1
        bit = (5 ^ exp - 1) `div` 2

toSnafu' :: Int -> Int -> String
toSnafu' 0 e = replicate (e + 1) '0'
toSnafu' n 0
  | n >= -2 && n <= 2 = [numToChar n]
  | otherwise = error $ printf "Impossible number: %d" n
toSnafu' n e =  numToChar res' : toSnafu' (n - res'') (e - 1)
  where n' = abs n
        one = 5 ^ e
        res
          | n' < one =
          if ((5 ^ e - 1) `div` 2) >= n' then 0 else 1
          | ((5 ^ e - 1) `div` 2 + one) >= n' = 1
          | otherwise = 2
        res' = signum n * res
        res'' = res' * one

toSnafu :: Int -> String
toSnafu n = toSnafu' n e
  where e = numChars n

solve = do
  converts <- lines <$> getContents
  printf "Sum of fuel: %s" (show $ toSnafu $ sum $ map fromSnafu converts)
