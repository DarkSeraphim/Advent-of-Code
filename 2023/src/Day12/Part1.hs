module Day12.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (many1, oneOf, char, sepBy1, sepEndBy1, endOfLine)
import Data.List (uncons)

data Condition = Working | Broken | Unknown deriving (Eq, Show)

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

countPossibilities :: [Condition] -> [Int] -> Int
countPossibilities conds broken
  | null broken = if Broken `elem` conds then 0 else 1
  -- Not enough places left for our broken pieces
  | length conds < sum broken + length broken - 1 = 0
  -- We're starting with working pieces, drop those
  | head conds == Working = countPossibilities (dropWhile (== Working) conds) broken
  | head conds == Broken = case m of
                             Just cs -> countPossibilities cs (tail broken)
                             Nothing -> 0
  -- Let's match a broken piece, and ensure we have a working piece after it
  | otherwise = (case m of
                     Just cs -> countPossibilities cs (tail broken)
                     _ -> 0
                  ) + countPossibilities (tail conds) broken
  where m = match conds (head broken)


solve :: IO ()
solve = do
  rows <- parseInput (pRow `sepEndBy1` endOfLine)
  printf "The amount of combinations they can be broken is %d\n" (sum $ map (uncurry (countPossibilities)) rows)
