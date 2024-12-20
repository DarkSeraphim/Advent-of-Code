module Day19.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (noneOf, many1, endOfLine, endBy1, string, sepBy1, count)
import Helpers.Parsec (Parser, parseInput)
import Data.Set (Set, fromList, member)
import Data.Map (empty, Map, insert, (!))
import qualified Data.Map as M
import Data.List (sortOn)

towel :: Parser String
towel = many1 $ noneOf ", \r\n"

towels :: Parser [String]
towels = towel `sepBy1` string ", "

designs :: Parser [String]
designs = many1 (noneOf "\r\n") `endBy1` endOfLine

question :: Parser ([String], [String])
question = (,) <$> towels <* count 2 endOfLine <*> designs

type Cache = Map String Bool

validate :: Cache -> Int -> String -> Set String -> (Cache, Bool)
validate cache _     []     _ = (cache, True)
validate cache m target avail
  | target `M.member` cache = (cache, cache ! target)
  | null next = (insert target False cache, False)
  | otherwise = (insert target res resCache, res)
  where towelSizes = [1..(min m (length target))]
        fits size' = take size' target `member` avail
        next = filter fits towelSizes
        rec cache' target' = validate cache' m target' avail
        foldF (cache', bool) n = (insert target' res'' cache'', res'' || bool)
          where (cache'', res'') = rec cache' target'
                target' = drop n target
        (resCache, res) = foldl foldF (cache, False) next
        

validateAndStore :: Int -> Set String -> Map String Bool -> String -> Map String Bool
validateAndStore maxSize availableTowels store target = cache
  where (cache, _) = validate store maxSize target availableTowels

solve :: IO ()
solve = do
  (availableTowels, targetDesigns) <- parseInput question
  let maxSize = maximum $ map length availableTowels
  let sorted = sortOn length targetDesigns
  let possible = foldl (validateAndStore maxSize (fromList availableTowels)) empty sorted
  printf "%d designs are possible\n" (length $ filter (possible !) targetDesigns)
