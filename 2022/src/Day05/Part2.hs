module Day05.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (anyChar, (<|>), string, sepBy1, endOfLine, endBy1, many, parserTrace)
import Text.Parsec.String (GenParser)
import Helpers.Parsec (Parser, parseInput, number, sepBy1', endBy1')
import Data.List (transpose, intercalate)
import Data.Maybe (catMaybes)
import Data.Map (fromList, Map, (!), insert)

type Crate = Char
data Move = Move {count :: Int, src :: Int, dst :: Int}

parseCrate :: Parser (Maybe Crate)
parseCrate = Just <$> (string "[" *> anyChar <* string "]") <|> Nothing <$ string "   "
parseCrateLine :: Parser [Maybe Crate]
parseCrateLine = parseCrate `sepBy1` string " "
parseCrateLines :: Parser [[Maybe Crate]]
parseCrateLines = endBy1' parseCrateLine endOfLine

parseNumber :: Parser Int
parseNumber = string " " *> number <* string " "

parseNumberLine :: Parser [Int]
parseNumberLine = (parseNumber `sepBy1` string " ") <* endOfLine

parseInstruction = Move <$> (string "move " *> number) <*> (string " from " *> number) <*> (string " to " *> number)
parseInstructions = parseInstruction `endBy1` endOfLine

parse = (,,) <$> parseCrateLines <*> parseNumberLine <* endOfLine <*> parseInstructions

prepareCrates crates = map catMaybes $ transpose crates

moveCrates :: Map Int [Crate] -> [Move] -> Map Int [Crate]
moveCrates piles [] = piles
moveCrates piles (move:rest) = moveCrates piles'' rest
  where s = piles ! src move
        d = piles ! dst move
        a = count move
        piles' = insert (src move) (drop a s) piles
        piles'' = insert (dst move) (take a s ++ d) piles'

solve = do
  (stacks', cols, instructions) <- parseInput parse
  let stacks = prepareCrates stacks'
  let piles = fromList $ zip cols stacks
  let result = moveCrates piles instructions
  let code = map (head . (result !)) cols
  printf "Top crates are %s" code
