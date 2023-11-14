module Day19.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (number, Parser, sepBy1', parseInput)
import Text.Parsec (string, many1, noneOf, endBy1)
import Text.Parsec.Char
import Data.Map (Map, fromList, unionWith, (!), insertWith, empty, findWithDefault, singleton, elems, member, toList, keys)
import qualified Data.Map as M
import Debug.Trace (trace)
import Helpers.List (combinations, combinations1)
import Data.Foldable (maximumBy)
import Data.List (sortBy, minimumBy)

data Resource = Geode | Obsidian | Clay | Ore deriving (Eq, Ord, Show)

data Req = Req {resource :: Resource, amount :: Int}
type Storage = Map Resource Int
type Bots = Map Resource Int

parseRequirement :: Parser Req
parseRequirement = do
  n <- number
  char ' '
  s <- many1 $ noneOf " ."
  return Req { amount = n, resource = case s of
           "ore" -> Ore
           "clay" -> Clay
           "obsidian" -> Obsidian
           _ -> error ("Invalid type " ++ s)}

parseRequirements :: Parser [Req]
parseRequirements = parseRequirement `sepBy1'` string " and "

parseFactory :: Parser [Req]
parseFactory = string " Each " <* many1 (noneOf " ") <* string " robot costs " *> parseRequirements <* char '.'

parseBlueprintData :: Parser (Map Resource [Req])
parseBlueprintData = do
  oreFactory <- parseFactory
  clayFactory <- parseFactory
  obsidianFactory <- parseFactory
  geodeFactory <- parseFactory
  return (fromList [(Ore, oreFactory), (Clay, clayFactory), (Obsidian, obsidianFactory), (Geode, geodeFactory)])

parseBlueprintHeader :: Parser Int
parseBlueprintHeader = string "Blueprint " *> number

parse :: Parser [(Int, Map Resource [Req])]
parse = ((,) <$> (parseBlueprintHeader <* string ":") <*> parseBlueprintData) `endBy1` endOfLine

-- We always want to do Geode if it's possible
edges :: [[Resource]]
edges = [[Geode], [Obsidian], [Clay], [Ore]]

meetsReqs :: [Req] -> Storage -> Bool
meetsReqs reqs resources = all (\r -> findWithDefault 0 (resource r) resources >= amount r) reqs

createBot :: Int -> Map Resource [Req] -> (Storage, Bots) -> Resource -> (Storage, Bots)
createBot time requirements (resources, newBots) target
  | meetsReqs reqs resources =  (resources', newBots')
--  | any (<0) (elems resources) = error $ printf "Invalid state: %d - %s - %s - %s " time (show target) (show resources) (show newBots)
  | otherwise = (resources, newBots)
  where reqs = requirements ! target
        sub a b = b - a
        resources' = foldl (\m r -> insertWith sub (resource r) (amount r) m) resources reqs
        newBots' = insertWith (+) target 1 newBots

-- I intentionally use compare b' a' to reverse the ordering
compareLoot :: Storage -> Storage -> Ordering
compareLoot a b = compare a' b'
  where a' = findWithDefault (-1) Geode a
        b' = findWithDefault (-1) Geode b

bestResource :: [[Resource]] -> Resource
bestResource resourcesList = maximum $ map maximum resourcesList

filterEdges :: Storage -> [[Req]] -> Bool
filterEdges storage reqs = all (\(r, a) -> findWithDefault 0 r storage >= a) $ toList m'
  where m = sumRequirements reqs
        m' = {-trace (show m) -}m

sumRequirements :: [[Req]] -> Map Resource Int
sumRequirements reqs = foldl (\m r -> insertWith (+) (resource r) (amount r) m) empty (concat reqs)

robotMade :: Resource -> [(Storage, Bots)] -> Bool
robotMade r = any (member r . snd)

runSimulation :: Map Resource [Req] -> Storage -> Bots -> Int -> [(Storage, Bots)]
runSimulation _ resources bots 0 = [(resources, bots)]
runSimulation requirements resources robots time = concatMap recurse possibles
  where atMostBots = keys $ M.filter (== 0) $ unionWith (-) robots (sumRequirements $ elems requirements)
        -- First filter all impossible edges based on our current resources
        edges' = filter (filterEdges resources . map (requirements ! )) edges
        best = bestResource edges'
        edges'' = trace (printf "Edges: %d\n" (length edges')) $ [] : filter (\es -> {-elem best es && -}not (any (`elem` atMostBots) es)) edges'
        -- [(resources', newBots)]
        possibles = map (foldl (createBot time requirements) (resources, empty)) edges''
        possibles' = if robotMade Geode possibles then filter (member Geode . snd) possibles else possibles
        recurse (resources', newBots) = runSimulation requirements resources'' robots' (time - 1)
          where resources'' = unionWith (+) resources' robots
                robots' = unionWith (+) robots newBots

solve = do
  blueprints <- parseInput parse
  let (id, reqs) = head blueprints
  let res' = runSimulation reqs empty (singleton Ore 1)
  printf "%s\n" (show $ map (length . res') [13,14,15,16, 17, 18, 19])
  --let res = head res'--maximumBy compareLoot res'
  --printf "Blueprints can generate %d geodes\n" (findWithDefault (-1) Geode res)
