module Day12.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (many1, oneOf, char, sepBy1, sepEndBy1, endOfLine)
import Data.List (uncons, intercalate)
import Debug.Trace (trace)
import Helpers.List (cycleN)
import Data.Map (Map, (!), member, insert, empty)

data Condition = Working | Broken | Unknown deriving (Eq, Show, Ord)

fromChar :: Char -> Condition
fromChar '.' = Working
fromChar '#' = Broken
fromChar '?' = Unknown
fromChar  c  = error ("Unknown Condition '" ++ [c, '\''])

pConditions :: Parser [Condition]
pConditions = many1 (fromChar <$> oneOf ".#?")

pBroken :: Parser [Int]
pBroken = number `sepBy1` char ','

pRow :: Parser ([Condition], [Int])
pRow = (,) <$> pConditions <* char ' ' <*> pBroken

match :: [Condition] -> Int -> Maybe [Condition]
match conds num
  | Working `notElem` shouldBeBroken = case next of
                                       Just (Broken, _) -> Nothing
                                       Just (_, t) -> Just t
                                       Nothing -> Just []
  | otherwise = Nothing

  where shouldBeBroken = take num conds
        next = uncons $ drop num conds

type Memory = Map ([Condition], [Int]) Int

countPossibilities :: Memory -> [Condition] -> [Int] -> (Memory, Int)
countPossibilities memory conds broken
  | (conds, broken) `member` memory = (memory, memory ! (conds, broken))
  | null broken = (memory, if Broken `elem` conds then 0 else 1)
  -- Not enough places left for our broken pieces
  | length conds < (sum broken + length broken - 1) = (memory, 0)
  -- We're starting with working pieces, drop those
  | head conds == Working = countPossibilities' memory (tail conds) broken
  | head conds == Broken = case m of
                             Just cs -> countPossibilities' memory cs (tail broken)
                             Nothing -> (memory, 0)
  -- Let's match a broken piece, and ensure we have a working piece after it
  | otherwise = (memory'', res' + res'')
  where m = match conds (head broken)
        (memory', res') = case m of
                Just cs -> countPossibilities' memory cs (tail broken)
                Nothing -> (memory, 0)
        (memory'', res'') = countPossibilities' memory' (tail conds) broken

countPossibilities' :: Memory -> [Condition] -> [Int] -> (Memory, Int)
countPossibilities' memory conds broken = (insert (conds, broken) res memory', res)
  where (memory', res) = countPossibilities memory conds broken

unfoldRow :: [Condition] -> [Int] -> ([Condition], [Int])
unfoldRow cs bs = (intercalate [Unknown] (replicate 5 cs), cycleN 5 bs)

solve :: IO ()
solve = do
  rows <- parseInput (pRow `sepEndBy1` endOfLine)
  printf "The amount of combinations they can be broken is %d\n" (sum $ map (snd . uncurry (countPossibilities empty) . uncurry unfoldRow) rows)
