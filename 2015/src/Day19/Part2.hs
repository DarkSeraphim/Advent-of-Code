module Day19.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (many1, letter, string, endOfLine)
import Helpers.Parsec
import Data.Char (isLower)
import Data.Maybe (fromJust)
import Helpers.List (head')
import Data.Foldable (find)
import Control.Applicative (asum)
import Data.List (sortOn)

moleculeP :: Parser [String]
moleculeP = atoms <$> many1 letter

replacementP :: Parser (String, [String])
replacementP = ((,) . head' <$> moleculeP) <*> (string " => " *> moleculeP)

replacementsP :: Parser [(String, [String])]
replacementsP = replacementP `endBy1'` endOfLine

inputP :: Parser ([(String, [String])], [String])
inputP = (,) <$> replacementsP <*> (endOfLine *> moleculeP <* endOfLine)

atoms :: String -> [String]
atoms [] = []
atoms (a:as) = nextAtom : atoms (dropWhile isLower as)
  where nextAtom = a : takeWhile isLower as

lastIndexOf :: Eq a => [a] -> [a] -> Maybe Int
lastIndexOf needle haystack = find (\i -> slice i == needle) poss
  where slice i = take (length needle) (drop i haystack)
        poss = reverse [0 .. (length haystack - length needle)]

matchReplacement :: [String] -> (String, [String]) -> Maybe (Int, [String], String)
matchReplacement molecule (a, b) = case x of
                                          Just i -> Just (i, b, a)
                                          Nothing -> Nothing
  where x = lastIndexOf b molecule

loop :: [String] -> [(String, [String])] -> Int
loop ["e"] _ = 0
loop molecule repls = 1 + loop molecule' repls
  where (idx, fin, repl) = fromJust $ asum $ map (matchReplacement molecule) repls
        molecule' = take idx molecule ++ [repl] ++ drop (idx + length fin) molecule


solve :: IO ()
solve = do
  (replacements, molecule) <- parseInput inputP
  let res = loop molecule (sortOn (\(_, as) -> -(length as)) replacements)
  printf "Fewest steps to get to molecule is %d\n" res
