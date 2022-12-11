module Day11.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (number, Parser, parseInput, numberInteger)
import Text.Parsec (string, sepBy1, char, (<|>), space, endOfLine, endBy1)
import Data.Map (fromList, Map, (!), insert, insertWith, unionsWith, empty, elems)
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

processMonkey' :: MonkeyAct -> Inventory -> Integer -> Inventory
processMonkey' (_, (op, operand), d, t, f) items item = insertWith append target [item'] items
  where item' = applyOperation op operand item `div` 3
        target = if item' `rem` d == 0 then t else f

lengthInteger :: [a] -> Integer 
lengthInteger a = toInteger $ length a

-- Process the monkey items (throw them) and set own items to an empty list
processMonkey :: (Inventory, MCount) -> MonkeyAct -> (Inventory, MCount)
processMonkey (items, counts) a@(mid, _, _, _, _) = (insert mid [] items', counts')
  where mi = items ! mid
        items' = foldl (processMonkey' a) items mi
        counts' = insertWith (+) mid (lengthInteger mi) counts

run :: MCount -> Inventory -> [MonkeyAct] -> Int -> [(Inventory, MCount)]
run counts map acts 0 = [(map, counts)]
run counts map acts round = (map, counts) : run counts' map' acts (round - 1)
  where (map', counts') = foldl processMonkey (map, counts) acts

solve = do
  monkeys <- parseInput parseMonkeys
  let items = fromList $ map fst monkeys
  let instr = map snd monkeys
  let res = product . take 2 . reverse . sort . elems . snd . last $ run empty items instr 20
  printf "It's monkey business! It's over %d!!!" res
