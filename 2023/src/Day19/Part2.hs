{-# LANGUAGE TupleSections #-}
module Day19.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (lower, many1, sepBy1, endBy1, endOfLine, oneOf, anyChar)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec.Char (char)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Text.Parsec.Prim (try)
import Data.Map (Map, fromList, (!), findWithDefault, insert)
import qualified Data.Map as M
import Prelude hiding (GT, LT)
import Helpers.Range (newRange, Range, size, getMax, getMin)

data Cmp = LT | GT
data Result = Jump String | Value Bool
data Expr = Cond ((Char, Cmp, Int), Result) | Res Result

type InputBlock = Map Char (Range Int)

pName :: Parser String
pName = many1 lower

pResult :: Parser Bool
pResult = char 'A' $> True <|> char 'R' $> False

pXMASvalue :: Parser Char
pXMASvalue = oneOf "xmas"

pCompare :: Parser Cmp
pCompare = (char '<' $> LT) <|> (char '>' $> GT)

pCond :: Parser (Char, Cmp, Int)
pCond = (,,) <$> pXMASvalue <*> pCompare <*> number

pJump :: Parser ((Char, Cmp, Int), Result)
pJump = (,) <$> (pCond <* char ':') <*> (Jump <$> pName <|> Value <$> pResult)

pWorkflow :: Parser (String, [Expr])
pWorkflow = (,) <$> (pName <* char '{') <*> ((try (Cond <$> pJump) <|> (Res <$> (Value <$> pResult) <|> (Res . Jump <$> pName))) `sepBy1` char ',') <* char '}'

pWorkflows :: Parser (Map String [Expr])
pWorkflows = fromList <$> (pWorkflow `endBy1` endOfLine)


combinations :: InputBlock -> Integer
combinations inp = product $ map (\k -> findWithDefault 0 k (M.map (toInteger . size) inp)) "xmas"

splitInput :: InputBlock -> Expr -> (Maybe InputBlock, Maybe InputBlock)
splitInput inp (Res _) = (Just inp, Nothing)
splitInput inp (Cond ((c, LT, i), _))
  | getMin v >= i = (Nothing, Just inp)
  | getMax v < i = (Just inp, Nothing)
  | otherwise = (Just $ insert c (newRange (getMin v) (i - 1)) inp, Just $ insert c (newRange i (getMax v)) inp)
  where v = inp ! c
splitInput inp (Cond ((c, GT, i), _))
  | getMax v <= i = (Nothing, Just inp)
  | getMin v > i = (Just inp, Nothing)
  | otherwise = (Just $ insert c (newRange (i + 1) (getMax v)) inp, Just $ insert c (newRange (getMin v) i) inp)
  where v = inp ! c

getResult :: Expr -> Result
getResult (Cond (_, res)) = res
getResult (Res res) = res

resolveResult :: Map String [Expr] -> Result -> InputBlock -> Integer
resolveResult _ (Value v) block = if v then combinations block else 0
resolveResult workflows (Jump str) block = resolveBlock workflows str block

resolveExprs :: Map String [Expr] -> [Expr] -> InputBlock -> Integer
resolveExprs _ [] _ = error "Last should always match the entire block"
resolveExprs workflows (e:es) inp = matchResult + maybe 0 (resolveExprs workflows es) left
  where (match, left) = splitInput inp e
        matchResult = maybe 0 (resolveResult workflows (getResult e)) match


resolveBlock :: Map String [Expr] -> String -> InputBlock -> Integer
resolveBlock workflows start = resolveExprs workflows exprs
  where exprs = workflows ! start

solve :: IO ()
solve = do
  workflows <- parseInput (pWorkflows <* endOfLine <* many1 anyChar)
  let range = newRange 1 4000
  let accepted = resolveBlock workflows "in" (fromList (map (, range) "xmas"))
  printf "The sum of accepted XMAS parts is %d\n" accepted
