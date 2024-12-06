module Day05.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, endOfLine, sepBy1, endBy1)
import Data.IntMap (fromList, IntMap, findWithDefault)

type Update = [Int]
type Rule = (Int, Int)

rule :: Parser Rule
rule = (,) <$> number <* char '|' <*> number

rules :: Parser [Rule]
rules = rule `endBy1` endOfLine

update :: Parser Update
update = number `sepBy1` char ','

updates :: Parser [Update]
updates = update `endBy1` endOfLine

document :: Parser ([Rule], [Update])
document = (,) <$> rules <* endOfLine <*> updates

validatePosition :: IntMap Int -> Rule -> Bool
validatePosition indexMap (first, second) = f < s
  where f = findWithDefault minBound first indexMap
        s = findWithDefault maxBound second indexMap

validateUpdate :: [Rule] -> Update -> Bool
validateUpdate rules' update' = all (validatePosition indexMap) rules'
  where indexMap = fromList $ zip update' [0 .. ]

getMiddle :: Update -> Int
getMiddle update' = update' !! (length update' `div` 2)

solve :: IO ()
solve = do
  (rules', updates') <- parseInput document
  let correct = filter (validateUpdate rules') updates'
  printf "Valid updates: %d\n" (sum $ map getMiddle correct) 
