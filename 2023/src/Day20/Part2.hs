{-# LANGUAGE TupleSections #-}
module Day20.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec (string, noneOf, many1, char, (<|>), endOfLine, sepEndBy1)
import Data.Functor (($>))
import Text.Parsec.Combinator (sepBy1)
import Data.Map (Map, fromList, empty, insert, findWithDefault, insertWith, singleton, union, (!), elems, toList, keys, size)
import qualified Data.Map as M
import Data.Set (Set, intersection)
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import Prelude hiding (null)

data ModType = Broadcaster | FlipFlop | Conjunction deriving Eq
type ModReg = Map String (ModType, [String])
type Incoming = Map String (Map String Bool)
type Memory = Map String Bool

data LinkedQueue a = LinkedQueue (Set (Integer, a))

nextId :: Set (Integer, a) -> Integer
nextId s
  | S.null s = 0
  | otherwise = 1 + fst (S.findMax s)

push :: Ord a => LinkedQueue a -> a -> LinkedQueue a
push (LinkedQueue q) a = LinkedQueue (S.insert (nextId q, a) q)
popFirst :: LinkedQueue a -> (a, LinkedQueue a)
popFirst (LinkedQueue q) = (snd a, LinkedQueue q')
  where (a, q') = S.deleteFindMin q

null :: LinkedQueue a -> Bool
null (LinkedQueue q) = S.null q

mkQueue :: a -> LinkedQueue a
mkQueue a = LinkedQueue (S.singleton (0, a))

pMod :: Parser (ModType, String)
pMod = (Broadcaster,) <$> string "broadcaster" <|> (,) <$> ((char '%' $> FlipFlop) <|> (char '&' $> Conjunction)) <*> many1 (noneOf " ")

pArrow :: Parser String
pArrow = string " -> "

pOutputs :: Parser [String]
pOutputs = many1 (noneOf ",\r\n") `sepBy1` string ", "

pModule :: Parser (String, (ModType, [String]))
pModule = do
  (typ, str) <- pMod
  _ <- pArrow
  outputs <- pOutputs
  return (str, (typ, outputs))

pModuleMap :: Parser ModReg
pModuleMap = fromList <$> pModule `sepEndBy1` endOfLine

handleSignal :: Bool -> ModType -> String -> Memory -> (Maybe String, Memory)
handleSignal signal Broadcaster name mem = (Just name, insert name signal mem)
handleSignal signal FlipFlop name mem
  | not signal = (Just name, insert name new mem)
  | otherwise = (Nothing, mem)
  where new = not (findWithDefault False name mem)
handleSignal signal Conjunction name mem = (Just name, insert name signal mem)

sendSignal :: Bool -> ModReg -> String -> Memory -> Incoming -> LinkedQueue String -> (Memory, Incoming, LinkedQueue String)
sendSignal signal reg name memory incoming queue = (memory', incoming', foldl push queue (reverse $ catMaybes new))
  where names = snd $ findWithDefault (Broadcaster, []) name reg
        incoming' = foldl (\m k -> insertWith union k (singleton name signal) m) incoming names
        (new, memory') = foldl f ([], memory) names
        f (newOut, memory''') name' = (out : newOut, memory'')
          where (out, memory'') = handleSignal signal (fst $ findWithDefault (Broadcaster, []) name' reg) name' memory'''

computeSignal :: ModType -> String -> Memory -> Incoming -> Bool
computeSignal Broadcaster name mem _ = findWithDefault False name mem
computeSignal FlipFlop name mem _ = findWithDefault False name mem
computeSignal Conjunction name _ inc = not $ and $ elems (inc ! name)

addIfHigh :: String -> Bool -> (Memory, Incoming, Set String) -> (Memory, Incoming, Set String)
addIfHigh name signal (mem, inc, highs)
  | signal = (mem, inc, S.insert name highs)
  | otherwise = (mem, inc, highs)

pressButton :: ModReg -> Memory -> Incoming -> LinkedQueue String -> (Memory, Incoming, Set String)
pressButton reg mem incoming queue
  | null queue = (mem, incoming, S.empty)
  | otherwise = addIfHigh next signal (pressButton reg mem' incoming' queue'')
  where (next, queue') = popFirst queue
        (ty, _) = findWithDefault (Broadcaster, []) next reg
        signal = computeSignal ty next mem incoming
        (mem', incoming', queue'') = sendSignal signal reg next mem incoming queue'

pressButtonNTimes :: Int -> Set String -> Map String Int -> Memory -> Incoming -> ModReg -> Map String Int
-- Add 1 for the initial button press
pressButtonNTimes n t firstOn mem incoming reg
  | size firstOn' == S.size t = firstOn'
  | otherwise = pressButtonNTimes (n + 1) t firstOn' mem' incoming' reg
  where (mem', incoming', highs) = pressButton reg mem incoming (mkQueue "broadcaster")
        newFound = fromList $ map (, n) $ S.toList (highs `intersection` t)
        firstOn' = firstOn `union` newFound

buildIncoming :: ModReg -> Incoming
buildIncoming reg = incomingKeys
  where cons = map fst $ filter ((==Conjunction) . fst . snd) $ toList reg
        f k = keys $ M.filter (elem k . snd) reg
        incomingKeys = fromList $ map (\c -> (c, fromList $ map (, False) $ f c)) cons

getInputs :: String -> Map String (a, [String]) -> [String]
getInputs target modules = keys $ M.filter (elem target . snd) modules

solve :: IO ()
solve = do
  modules <- parseInput pModuleMap
  let memory = empty
  let incoming = buildIncoming modules
  let t = S.fromList $ concatMap (`getInputs` modules) (getInputs "rx" modules)
  let signals = pressButtonNTimes 1 t empty memory incoming modules
  let sol = foldl1 lcm $ elems signals
  printf "Product of the waves is %s\n" (show sol)
