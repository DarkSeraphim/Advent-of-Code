{-# LANGUAGE TupleSections #-}
module Day15.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseInput, number)
import Text.Parsec (noneOf, sepBy1, char, many1, (<|>))
import Data.Char (ord)
import Control.Applicative ((<**>))
import Data.IntMap (fromList, IntMap, (!), insert, update, toList)

data Inst = Upsert Int String | Remove String
type LensMap = IntMap [(String, Int)]
type Entry = (String, Int)

pInst :: Parser Inst
pInst = many1 (noneOf "-=\r\n,") <**> ((Upsert <$> (char '=' *> number)) <|> (Remove <$ char '-'))

pSteps :: Parser [Inst]
pSteps = pInst `sepBy1` char ','

updateHash :: Int -> Char -> Int
updateHash n c = ((n + ord c) * 17) `mod` 256

hash :: String -> Int
hash = foldl updateHash (0 :: Int)

replace :: [Entry] -> Entry -> [Entry]
replace [] e = [e] -- insert
replace (o:es) e
  | fst o == fst e = e : es -- update
  | otherwise      = o : replace es e -- continue

processStep :: LensMap -> Inst -> LensMap
processStep m (Remove label) = update (Just . filter ((/= label) . fst)) (hash label) m
processStep m (Upsert num label) = insert h (replace es (label, num)) m
  where h = hash label
        es = m ! h


emptyLensMap :: LensMap
emptyLensMap = fromList $ map (, []) [0..255]

scoreBox :: (Int, [Entry]) -> [Int]
scoreBox (box, lenses) = map ((box + 1) * ) $ zipWith (*) [1..] $ map snd lenses

scoreBoxes :: LensMap -> [Int]
scoreBoxes m = concatMap scoreBox $ toList m

solve :: IO ()
solve = do
  steps <- parseInput pSteps
  let boxes = foldl processStep emptyLensMap steps
  printf "The sum of the hashes is %d\n" (sum $ scoreBoxes boxes)
