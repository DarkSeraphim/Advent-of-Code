module Day19.Part1 (solve, atoms, allReplacements) where
import Text.Printf (printf)
import Text.Parsec (many1, letter, string, endOfLine)
import Helpers.Parsec
import Data.Set (Set, fromList, unions)
import Data.Char (isLower)
import Data.Maybe (mapMaybe)
import Helpers.List (head')

moleculeP :: Parser [String]
moleculeP = atoms <$> many1 letter

replacementP :: Parser (String, [String])
replacementP = ((,) . head' <$> moleculeP) <*> (string " => " *> moleculeP)

replacementsP :: Parser [(String, [String])]
replacementsP = replacementP `endBy1'` endOfLine

inputP :: Parser ([(String, [String])], [String])
inputP = (,) <$> replacementsP <*> (endOfLine *> moleculeP <* endOfLine)

allReplacements :: [String] -> (String, [String]) -> Set String
allReplacements as (atom, replacement) = fromList (mapMaybe replace [0 .. (length as - 1)])
  where replace i = if a == atom then Just (concat $ take i as ++ replacement ++ drop (i + 1) as) else Nothing
          where a = as !! i

atoms :: String -> [String]
atoms [] = []
atoms (a:as) = nextAtom : atoms (dropWhile isLower as)
  where nextAtom = a : takeWhile isLower as


solve :: IO ()
solve = do
  (replacements, molecule) <- parseInput inputP
  let allDifferent = unions $ map (allReplacements molecule) replacements
  printf "We have %d different molecules\n" (length allDifferent)
