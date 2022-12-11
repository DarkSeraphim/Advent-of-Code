module Day11.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (number, Parser, parseInput, numberInteger)
import Text.Parsec (string, sepBy1, char, (<|>), space, endOfLine, endBy1)
import Data.Map (fromList, Map, (!), insert, insertWith, unionsWith, empty, elems, unionWith)
import qualified Data.Map as M
import Helpers.List (cycleN)
import Data.List (sort)

data Operand = Old | Value Integer deriving Show
data Op = Mult | Plus deriving Show
type MonkeyAct = (Int, (Op, Operand), Integer, Int, Int)
type Inventory = Map Int [Integer]
type MCount = Map Int Integer

parseHeader = string "Monkey " *> number <* char ':'
parseStart = string "  Starting items: " *> (numberInteger `sepBy1` string ", ")
parseOp :: Parser Op
parseOp = Plus <$ char '+' <|> Mult <$ char '*'
parseOpand :: Parser Operand
parseOpand = Old <$ string "old" <|> Value <$> numberInteger
parseOperation :: Parser (Op, Operand)
parseOperation = (,) <$> (string "  Operation: new = old " *> (parseOp <* space)) <*> parseOpand
parseTest = string "  Test: divisible by " *> numberInteger
parseCondTrue = string "    If true: throw to monkey " *> number
parseCondFalse = string "    If false: throw to monkey " *> number

parseMonkey :: Parser ((Int, [Integer]), MonkeyAct)
parseMonkey = do
    h <- parseHeader <* endOfLine
    s <- parseStart <* endOfLine
    o <- parseOperation <* endOfLine
    t <- parseTest <* endOfLine
    ift <- parseCondTrue <* endOfLine
    iff <- parseCondFalse <* endOfLine
    return ((h, s), (h, o, t, ift, iff))

parseMonkeys = parseMonkey `sepBy1` endOfLine

applyOperation :: Op -> Operand -> Integer -> Integer
applyOperation Plus Old o = o + o
applyOperation Plus (Value v) o = o + v
applyOperation Mult Old o = o * o
applyOperation Mult (Value v) o = o * v

append :: [a] -> [a] -> [a]
append b c = reverse $ b ++ reverse c

processMonkey' :: MonkeyAct -> Integer -> Inventory -> Integer -> Inventory
processMonkey' (_, (op, operand), d, t, f) mod items item = insertWith append target [item'] items
  where item' = applyOperation op operand item `rem` mod
        target = if item' `rem` d == 0 then t else f

lengthInteger :: [a] -> Integer 
lengthInteger a = toInteger $ length a

-- Process the monkey items (throw them) and set own items to an empty list
processMonkey :: Integer -> (Inventory, MCount) -> MonkeyAct -> (Inventory, MCount)
processMonkey mod (items, counts) a@(mid, _, _, _, _) = (insert mid [] items', counts')
  where mi = items ! mid
        items' = foldl (processMonkey' a mod) items mi
        counts' = insertWith (+) mid (lengthInteger mi) counts

run :: Inventory -> [MonkeyAct] -> Integer -> Int -> MCount
run map acts mod 0 = empty
run map acts mod round = unionWith (+) counts' $ run map' acts mod (round - 1)
  where (map', counts') = foldl (processMonkey mod) (map, empty) acts

getDiv :: MonkeyAct -> Integer
getDiv (_, _, d, _, _) = d

solve = do
  monkeys <- parseInput parseMonkeys
  let items = fromList $ map fst monkeys
  let instr = map snd monkeys
  let mod = product (map getDiv instr)
  let res = product . take 2 . reverse . sort . elems $ run items instr mod 10000
  printf "It's monkey business! It's over %d!!!" res
