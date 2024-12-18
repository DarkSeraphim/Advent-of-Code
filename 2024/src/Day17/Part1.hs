module Day17.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (number, Parser, parseInput)
import Text.Parsec (string, char, sepBy1, endOfLine)
import Data.Bits (xor)
import Data.IntMap (IntMap, (!?), fromList)
import Helpers.List (chunksOf)
import Data.List (intercalate)
import Debug.Trace (trace)

register :: Char -> Parser Int
register c = string (printf "Register %s: " [c]) *> number <* endOfLine

program :: Parser [Int]
program = string "Program: " *> (number `sepBy1` char ',')

all' :: Parser ((Int, Int, Int), [Int])
all' = (,) <$> ((,,) <$> a <*> b <*> c) <*> (endOfLine *> program <* endOfLine)
  where a = register 'A'
        b = register 'B'
        c = register 'C'

type Registers = (Int, Int, Int)
data State = State {regs :: Registers, out :: [Int], ip :: Int}

combo :: Int -> Registers -> Int
combo 4 (a, _, _) = a
combo 5 (_, b, _) = b
combo 6 (_, _, c) = c
combo 7         _ = error "Reserved operand 7"
combo n         _
  | n < 4         = n
  | otherwise     = error "Bad operand"

eval :: Int -> Int -> State -> State
-- adv
eval 0 opand state@(State regs'@(a, b, c) _ _) = state {regs = (a `div` denom, b, c)}
  where denom = 2 ^ combo opand regs'
-- bxl
eval 1 opand state@(State (a, b, c) _ _) = state {regs = (a, b `xor` opand, c)}
-- bst
eval 2 opand state@(State regs'@(a, _, c) _ _) = state {regs = (a, combo opand regs' `mod` 8, c)}
-- jnz
eval 3 opand state@(State (a, _, _) _ _) = if a /= 0 then state {ip = opand} else state
-- bxc
eval 4     _ state@(State (a, b, c) _ _) = state {regs = (a, b `xor` c, c)}
-- out
eval 5 opand state@(State regs' out' _) = state {out = out''}
  where out'' = (combo opand regs' `mod` 8) : out'
-- bdv
eval 6 opand state@(State regs'@(a, _, c) _ _) = state {regs = (a, a `div` denom, c)}
  where denom = 2 ^ combo opand regs'
-- cdv
eval 7 opand state@(State regs'@(a, b, _) _ _) = state {regs = (a, b, a `div` denom)}
  where denom = 2 ^ combo opand regs'
-- error
eval _ _ _ = error "Malformed program"

run :: IntMap (Int, Int) -> State -> State
run program' state =
  case instr of
    Just (opcode, opand) -> trace (printf "opcode %d, operand %d" opcode opand) $ run program' $ eval opcode opand state'
    Nothing -> trace "nop" state
  where instr = program' !? ip state
        -- Prematurely bump IP, and overwrite this when jumping
        state' = state { ip = ip state + 2}

toTuple :: [Int] -> (Int, Int)
toTuple [a, b] = (a, b)
toTuple      _ = error "Bad list"

solve :: IO ()
solve = do
  (registers, program') <- parseInput all'
  let programMap = fromList $ zipWith (\a b -> (a, toTuple b)) (filter even [0..]) $ chunksOf 2 program'

  let result = run programMap (State {regs = registers, ip = 0, out = []})
  printf "Registers: %s\n" (show $ regs result)
  printf "The output is: %s\n" (intercalate "," (map show $ reverse $ out result))
