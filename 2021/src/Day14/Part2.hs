module Day14.Part2 (solve) where
    import Text.ParserCombinators.Parsec (upper, GenParser, endBy1, string, parse)

    import Text.Printf (printf)
    import Text.Parsec.Char (endOfLine)
    import Control.Applicative (some)
    import Helpers.Input (orFail)
    import Data.Map (fromList, Map, findWithDefault, toList, lookup, insertWith, empty)
    import Data.List (group, sort, groupBy)
    import Control.Arrow ((&&&))

    parseInput :: GenParser Char st (String, [((Char, Char), Char)])
    parseInput = (,) <$> template <* endOfLine <* endOfLine <*> insertions
        where template = some upper
              insertions = parseInsertion `endBy1` endOfLine

    parseInsertion :: GenParser Char st ((Char, Char), Char)
    parseInsertion = (,) <$> ((,) <$> upper <*> upper) <* string " -> " <*> upper

    toList' :: (Char, Char) -> String
    toList' (a, b) = [a, b]

    getNewPairs :: Map (Char, Char) Char -> ((Char, Char), Int) -> [((Char, Char), Int)]
    getNewPairs replacements ((a, b), c) = case insert' of
      Just newChar -> [((a, newChar), c), ((newChar, b), c)]
      Nothing -> [((a, b), c)]
      where insert' = Data.Map.lookup (a, b) replacements

    merge :: Ord k => (v -> v -> v) -> Map k v -> (k, v) -> Map k v
    merge f m (k, v) = insertWith f k v m

    apply :: Map (Char, Char) Int -> Map (Char, Char) Char -> Map (Char, Char) Int
    apply template replacements = foldl (merge (+)) empty x 
        where x = concatMap (getNewPairs replacements) $ toList template

    modify :: ((Char, Char), Int) -> [(Char, Int)]
    modify ((a, b), c) = [(a, c), (b, c)]

    flipTuple :: (a, b) -> (b, a)
    flipTuple (a, b) = (b, a)

    solve = do
        (template, insertionsList) <- orFail . parse parseInput "Input" =<< getContents
        let insertions = fromList insertionsList
        let pairs = fromList $ map (head &&& length) $ group $ sort $ zip template (drop 1 template)
        let final = foldl apply pairs (replicate 40 insertions)
        let finalPairs = foldl (merge (+)) empty $ concatMap modify $ toList final

        
        let frequencies = map flipTuple $ toList finalPairs
        let (maxValue, _) = maximum frequencies
        let (minValue, _) = minimum frequencies
        -- Off by 1 lmao
        printf "The difference was %d" ((maxValue `div` 2) - (minValue `div` 2) + 1)
