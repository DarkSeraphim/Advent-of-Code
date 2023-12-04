module Day03.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (SourcePos, getPosition, sourceLine, sourceColumn, noneOf, char, (<|>), try, many1, sepEndBy1, endOfLine)
import Helpers.Parsec (StatefulParser, parseInputWithState)
import Text.Parsec.Pos (newPos)
import Data.List (singleton)
import Data.Map (Map, fromList, partition, keys, lookup, elems, filter)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Helpers.Input (readInt)
import Text.Parsec.Char (digit)
import qualified Data.List

type Point = (Int, Int)
type Part = (Point, String)
type StablePart = (Point, String, Int)

add :: Point -> Point -> Point
add (a, b) (c, d) = (a + c, b + d)

parseCoordinated :: StatefulParser SourcePos String -> StatefulParser SourcePos Part
parseCoordinated parser = do
  pos <- getPosition
  a <- parser
  let y = sourceLine pos
  let x = sourceColumn pos
  return ((x, y), a)

parsePart :: StatefulParser SourcePos Part
parsePart = parseCoordinated (many1 digit)

parseAttachment :: StatefulParser SourcePos Part
parseAttachment = parseCoordinated (singleton <$> noneOf ".\r\n")

parseEmpty :: StatefulParser SourcePos Part
parseEmpty = parseCoordinated (singleton <$> char '.')

parseCellLine :: StatefulParser SourcePos [Part]
parseCellLine = many1 (try parsePart <|> try parseAttachment <|> parseEmpty)

parseCells :: StatefulParser SourcePos [Part]
parseCells = concat <$> (parseCellLine `sepEndBy1` endOfLine)


makeStable :: [Part] -> [StablePart]
makeStable parts = zipWith (\(x, y) z -> (x, y, z)) parts [0..]

expandPart :: StablePart -> [StablePart]
expandPart ((x, y), s, i) = map (\r' -> ((x + r', y), s, i)) r
  where l = length s
        r = [0 .. l - 1]

buildPartMap :: [StablePart] -> Map Point StablePart
buildPartMap parts = fromList (map (\(x, y, z) -> (x, (x, y, z))) parts)

allSurrounding :: Point -> [Point]
allSurrounding point = map (add point) allCoords
  where allCoords = [(x, y) | x <- [-1..1], y <- [-1..1], (x,y) /= (0,0)]

solve :: IO ()
solve = do
  points <- parseInputWithState parseCells (newPos "Input" 0 0)
  let validCells = Data.List.filter (\x -> "." /= snd x) points
  let expanded = concatMap expandPart (makeStable validCells)
  let partMap = buildPartMap expanded
  let (parts, attachments) = partition (\(_, s, _) -> isDigit $ head s) partMap
  let gears = Data.Map.filter (\(_, s, _) -> s == "*") attachments
  let allPoints = map (mapMaybe (`Data.Map.lookup` parts) . allSurrounding) (keys gears)
  let uniqued = map (fromList . map (\(_, s, i) -> (i, s))) allPoints
  let validGears = Data.List.filter (\x -> 2 == length x) uniqued
  let ratios = map (product . map readInt . elems) validGears


  printf "Sum of gear ratio is %d" (sum ratios)
