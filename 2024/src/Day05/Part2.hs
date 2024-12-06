module Day05.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (char, endOfLine, sepBy1, endBy1)
import Data.IntMap (fromList, IntMap, findWithDefault, fromListWith)
import Data.IntSet (IntSet, unions)
import qualified Data.IntSet as S
import Data.List (sort)
import Debug.Trace (trace)

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

findAllAfter :: IntMap [Int] -> Int -> IntSet
findAllAfter edges cur = S.fromList next `S.union` future
  where next = findWithDefault [] cur edges
        future = unions $ map (findAllAfter edges) next

fix :: [Rule] -> Update -> Update
fix rules' update' = map snd topo
  where validRules = filter (\(a, b) -> a `elem` update' && b `elem` update') rules'
        edgeMap = fromListWith (++) $ map (\(a, b) -> (a, [b])) validRules
        nums = unions $ map (\(a, b) -> S.fromList [a, b]) validRules
        topo = sort $ map (\n -> (S.size $ findAllAfter edgeMap n, n)) (S.toList nums)

getMiddle :: Update -> Int
getMiddle update' = update' !! (length update' `div` 2)

solve :: IO ()
solve = do
  (rules', updates') <- parseInput document
  let incorrect = filter (not . validateUpdate rules') updates'
  let correct = map (fix rules') incorrect
  let correct' = trace (show correct) correct
  printf "Valid updates: %d\n" (sum $ map getMiddle correct')
