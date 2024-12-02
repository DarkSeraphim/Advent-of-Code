module Day02.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec ( Parser, number, parseInput, spaces )
import Text.Parsec (endOfLine, sepBy1, sepEndBy1)

report :: Parser [Int]
report = number `sepBy1` spaces

reports :: Parser [[Int]]
reports = report `sepEndBy1` endOfLine

generateCombinations :: [Int] -> [[Int]]
generateCombinations arr = map (del arr) idxs
  where idxs = [0 .. (length arr - 1)]
        del a i = take i a ++ drop (i + 1) a 

isValidStep :: Int -> Int -> Bool
isValidStep a b = diff >= 1 && diff <= 3
  where diff = abs (a - b)

validate'' :: Bool -> [Int] -> Bool
validate'' _ [ ] = True
validate'' _ [_] = True
validate'' desc (a:b:c)
  | (a > b) == desc && isValidStep a b = validate'' desc (b:c)
  | otherwise = False


validate' :: [Int] -> Bool
validate' arr = validate'' False arr || validate'' True arr

validate :: [Int] -> Bool
validate arr = any validate' arrs
  where arrs = generateCombinations arr

solve :: IO ()
solve = do
  reports' <- parseInput reports
  let good = filter validate reports'
  printf "Valid report count after dampening: %d\n" (length good)
