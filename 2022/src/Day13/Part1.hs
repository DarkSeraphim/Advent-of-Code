module Day13.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput, sepBy1')
import Text.Parsec (endOfLine, sepBy1, (<|>), char, sepBy)
import Debug.Trace (trace)
import Data.List (find)
import Data.Maybe (fromMaybe)

data Node = List [Node] | Value Int
data Correct = Yes | No | Unknown deriving (Eq, Show)

parseList :: Parser Node
parseList = List <$> (char '[' *> (parseNode `sepBy` char ',') <* char ']')

parseValue :: Parser Node
parseValue = Value <$> number

parseNode :: Parser Node
parseNode = parseList <|> parseValue

parse :: Parser [(Node, Node)]
parse = ((,) <$> (parseNode <* endOfLine) <*> parseNode) `sepBy1'` (endOfLine <* endOfLine) <* endOfLine

validateLists' :: Node -> Node -> Correct
validateLists' (Value a) (Value b)
  | a == b = Unknown
  | a < b = Yes
  | otherwise = No
validateLists' la@(List _) b@(Value _) = validateLists' la (List [b])
validateLists' a@(Value _) lb@(List _) = validateLists' (List [a]) lb
validateLists' (List la) (List lb) = fromMaybe lengthCorrectness correctness
  where correctness = find (/= Unknown) $ zipWith (curry validateLists) la lb
        lla = length la
        llb = length lb
        lengthCorrectness
          | lla == llb = Unknown
          | lla > llb = No
          | otherwise = Yes

validateLists :: (Node, Node) -> Correct
validateLists = uncurry validateLists'

solve = do
  pairs <- parseInput parse
  let valid = map fst $ filter (\(i, r) -> r == Yes) $ zip [(1 :: Int)..] (map validateLists pairs)
  printf "Valid order is %d" (sum valid) 
