module Day19.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (lower, many1, sepBy1, endBy1, endOfLine, sepEndBy1)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec.Char (char)
import Control.Applicative ((<|>))
import Data.Functor (($>))
import Text.Parsec.Prim (try)
import Data.Map (Map, fromList, (!))
import Text.Parsec (string)
import Control.Monad (msum)
import Data.Maybe (fromJust)

data XMAS = XMAS {x :: Int, m :: Int, a :: Int, s :: Int}
data Result = Jump String | Value Bool
data Expr = Cond (XMAS -> Bool, Result) | Res Result

pName :: Parser String
pName = many1 lower

pResult :: Parser Bool
pResult = char 'A' $> True <|> char 'R' $> False

pXMASvalue :: Parser (XMAS -> Int)
pXMASvalue = char 'x' $> x <|> char 'm' $> m <|> char 'a' $> a <|> char 's' $> s

pCompare :: Parser (Int -> Bool)
pCompare = flip <$> ((char '<' $> (<)) <|> (char '>' $> (>))) <*> number

pCond :: Parser (XMAS -> Bool)
pCond = flip (.) <$> pXMASvalue <*> pCompare

pJump :: Parser (XMAS -> Bool, Result)
pJump = (,) <$> (pCond <* char ':') <*> (Jump <$> pName <|> Value <$> pResult)

pWorkflow :: Parser (String, [Expr])
pWorkflow = (,) <$> (pName <* char '{') <*> ((try (Cond <$> pJump) <|> (Res <$> (Value <$> pResult) <|> (Res . Jump <$> pName))) `sepBy1` char ',') <* char '}'

pWorkflows :: Parser (Map String [Expr])
pWorkflows = fromList <$> (pWorkflow `endBy1` endOfLine)

pXMAS :: Parser XMAS
pXMAS = XMAS <$> (string "{x=" *> number) <*> (string ",m=" *> number) <*> (string ",a=" *> number) <*> (string ",s=" *> number <* char '}')

pXMASes :: Parser [XMAS]
pXMASes = pXMAS `sepEndBy1` endOfLine

runWorkflowPart :: XMAS -> Expr -> Maybe Result
runWorkflowPart xmas (Cond (f, res))
  | f xmas = Just res
  | otherwise = Nothing
runWorkflowPart _ (Res result) = Just result

runWorkflow :: XMAS -> [Expr] -> Result
-- msum returning a Nothing is an error
runWorkflow xmas es = fromJust $ msum $ map (runWorkflowPart xmas) es

resolveXMAS :: Map String [Expr] -> String -> XMAS -> Bool
resolveXMAS workflows start xmas = accepted
  where exprs = workflows ! start
        result = runWorkflow xmas exprs
        accepted = case result of
                    Jump str -> resolveXMAS workflows str xmas
                    Value b -> b

score :: XMAS -> Int
score v = x v + m v + a v + s v

solve :: IO ()
solve = do
  (workflows, xmases) <- parseInput ((,) <$> (pWorkflows <* endOfLine) <*> pXMASes)
  let accepted = filter (resolveXMAS workflows "in") xmases
  printf "The sum of accepted XMAS parts is %d\n" (sum $ map (score) accepted)
