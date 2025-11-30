module Day24.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec ( Parser, number, spaces, parseInput, endBy1' )
import Text.Parsec (noneOf, endOfLine, many1, string, endBy1)
import Data.Map (fromList, Map, (!), insert, filterWithKey, keys)
import Data.Set (Set, member, union)
import Data.List (partition)

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Bits (Bits(shiftL))

bool :: Parser Bool
bool = (== 1) <$> number

label :: Parser String
label = many1 (noneOf ": \r\n")

input :: Parser (String, Bool)
input = (,) <$> label <*> (string ": " *> bool)

inputs :: Parser [(String, Bool)]
inputs = input `endBy1` endOfLine

toOp :: String -> Op
toOp "XOR" = XOR
toOp "OR" = OR
toOp "AND" = AND
toOp   op' = error ("Invalid op: " ++ op')

op :: Parser Op
op = toOp <$> many1 (noneOf " ")

arrow :: Parser String
arrow = string "->"

wire :: Parser Wire
wire = (,,,) <$> (label <* spaces) <*> (op <* spaces) <*> (label <* spaces) <*> (arrow *> spaces *> label)

wires :: Parser [Wire]
wires = wire `endBy1` endOfLine


machine :: Parser ([Input], [Wire])
machine = (,) <$> (inputs <* endOfLine) <*> wires

type Input = (String, Bool)
type Wire = (String, Op, String, String)
data Op = OR | XOR | AND

sortWires :: Set String -> [Wire] -> [Wire]
sortWires       _ [] = []
sortWires visited wires' = readyWires ++ sortWires visited' nextWires
  where ready (a, _, b, _) = a `member` visited && b `member` visited
        (readyWires, nextWires) = partition ready wires'
        visited' = visited `union` S.fromList (map (\(_, _, _, r) -> r) readyWires)

applyOp :: Op -> Bool -> Bool -> Bool
applyOp XOR x y = x /= y
applyOp  OR x y = x || y
applyOp AND x y = x && y

applyWire :: Map String Bool -> [Wire] -> Map String Bool
applyWire register [] = register
applyWire register ((a, op', b, r):ws) = applyWire register' ws
  where register' = insert r (applyOp op' va vb) register
        va = register ! a
        vb = register ! b

output :: Map String Bool -> Int
output register = sum $ map (\b -> 1 `shiftL` b) goodBits 
  where zs = filterWithKey (\k _ -> take 1 k == "z") register
        goodZs = M.filter id zs
        goodBits :: [Int]
        goodBits = map (read . drop 1) $ keys goodZs

solve :: IO ()
solve = do
  (inputs', wires') <- parseInput machine
  let register = fromList inputs'
      -- Toposort the machine
      sortedWires = sortWires (S.fromList (map fst inputs')) wires'
  let register' = applyWire register sortedWires
  printf "The decimal value is %d\n" (output register')
