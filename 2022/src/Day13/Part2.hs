module Day13.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput, sepBy1')
import Text.Parsec (endOfLine, sepBy1, (<|>), char, sepBy)
import Debug.Trace (trace)
import Data.List (find, sortBy)
import Data.Maybe (fromMaybe)

data Node = List [Node] | Value Int deriving Eq
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

dividerPackets :: [Node]
dividerPackets = [List [List [Value 2]], List [List [Value 6]]]

isDivider :: Node -> Bool
isDivider (List [List [Value n]]) = n == 2 || n == 6
isDivider _ = False

unpack :: (a, a) -> [a]
unpack (a, b) = [a, b]

solve = do
  pairs <- parseInput parse
  let allPackets = dividerPackets ++ concatMap unpack pairs
  printf "Got %d packets" (length allPackets)
  let bigger = map (\a -> (length $ filter (\b -> Yes == validateLists' a b) allPackets, a)) allPackets
  let b' = sortBy (\(la, a) (lb, b) -> compare (-la) (-lb)) bigger
  let b'' = zip [(1::Int)..] $ map snd b'
  let b''' = product $ map fst $ filter (\(i, n) -> isDivider n) b''
  printf "Valid order is %d" (b''')
