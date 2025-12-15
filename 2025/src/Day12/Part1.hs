module Day12.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec
import Helpers.Parsec

type Region = (Int, Int)

identifierP :: Parser Int
identifierP = number <* char ':' <* endOfLine

presentP :: Parser [String]
presentP = many1 (oneOf "#.") `endBy1` endOfLine

presentsP :: Parser [[String]]
presentsP = (identifierP *> presentP) `endBy1'` endOfLine

regionP :: Parser (Int, Int)
regionP = (,) <$> number <* char 'x' <*> number

amountsP :: Parser [Int]
amountsP = number `sepBy1` char ' '

requestsP :: Parser [(Region, [Int])]
requestsP = ((,) <$> regionP <* string ": " <*> amountsP) `endBy1` endOfLine

inputP :: Parser ([[String]], [(Region, [Int])])
inputP = (,) <$> presentsP <*> requestsP

countTiles :: [String] -> Int
countTiles = sum . map (length . filter (=='#'))

solveForGrid :: [[String]] -> (Region, [Int]) -> Bool
solveForGrid a ((w, h), counts) = fitsAtAll
  where fitsAtAll = (w * h) >= sum (zipWith (*) counts $ map countTiles a)

solve :: IO ()
solve = do
  (gifts, requests) <- parseInput inputP
  let withSolution = filter (solveForGrid gifts) requests
  printf "Original list: %d\n\n" (length requests)
  printf "We can fit %d requests\n" (length withSolution)
