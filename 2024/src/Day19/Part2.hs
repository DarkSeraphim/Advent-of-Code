module Day19.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (noneOf, many1, endOfLine, endBy1, string, sepBy1, count)

import Helpers.Parsec (Parser, parseInput)
import Data.Set (Set, fromList, member)
import Data.Map (empty, Map, insert, (!), findWithDefault)
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

type Cache = Map String Int

-- [] is an empty list
-- empty list implies no results
-- [[]] is not an empty list (but a list with an empty list inside)
-- so there is a solution, but we are at the end of target
validate :: Cache -> Int -> String -> Set String -> (Cache, Int)
validate cache _     []     _ = (cache, 1)
validate cache m target avail
  | target `M.member` cache = (cache, cache ! target)
  | null next = (insert target 0 cache, 0)
  | otherwise = (insert target res resCache, res)
  where towelSizes = [1..(min m (length target))]
        fits size' = take size' target `member` avail
        next = filter fits towelSizes
        rec :: Cache -> String -> (Cache, Int)
        rec cache' target' = validate cache' m target' avail
        foldF :: (Cache, Int) -> Int -> (Cache, Int)
        foldF (cache', bool) n = (insert target' res'' cache'', bool + res'')
          where (cache'', res'') = rec cache' target'
                target' = drop n target
        (resCache, res) = foldl foldF (cache, 0) next


validateAndStore :: Int -> Set String -> Map String Int -> String -> Map String Int
validateAndStore maxSize availableTowels store target = cache
  where (cache, _) = validate store maxSize target availableTowels

solve :: IO ()
solve = do
  (availableTowels, targetDesigns) <- parseInput question
  let maxSize = maximum $ map length availableTowels
  let sorted = sortOn length targetDesigns
  let possible = foldl (validateAndStore maxSize (fromList availableTowels)) empty sorted
  printf "%d designs are good\n" (length $ filter (\k -> findWithDefault 0 k possible /= 0) targetDesigns)
  printf "%d designs are possible\n" (sum $ map (\k -> findWithDefault 0 k possible) targetDesigns)
