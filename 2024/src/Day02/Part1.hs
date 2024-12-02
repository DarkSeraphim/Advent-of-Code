module Day02.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (number, spaces, Parser, parseInput)
import Text.Parsec (endOfLine, sepBy1, sepEndBy1)

report :: Parser [Int]
report = number `sepBy1` spaces

reports :: Parser [[Int]]
reports = report `sepEndBy1` endOfLine

isDescending :: [Int] -> Bool
isDescending [] = False
isDescending [_] = False
isDescending (a:b:_) = a > b

isValidStep :: Int -> Int -> Bool
isValidStep a b = diff >= 1 && diff <= 3
  where diff = abs (a - b)

validate' :: Bool -> [Int] -> Bool
validate' _ [ ] = True
validate' _ [_] = True
validate' desc (a:b:c)
  | (a > b) == desc && isValidStep a b = validate' desc (b:c)
  | otherwise = False

validate :: [Int] -> Bool
validate arr = validate' (isDescending arr) arr

solve :: IO ()
solve = do
  reports' <- parseInput reports
  let res = filter validate reports'
  printf "Valid report count: %d" (length res)
