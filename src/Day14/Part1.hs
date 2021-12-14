module Day14.Part1 (solve) where
    import Text.ParserCombinators.Parsec (upper, GenParser, endBy1, string, parse)

    import Text.Printf (printf)
    import Text.Parsec.Char (endOfLine)
    import Control.Applicative (some)
    import Helpers.Input (orFail)
    import Data.Map (fromList, Map, findWithDefault)
    import Data.List (group, sort)
    import Control.Arrow ((&&&))

    parseInput :: GenParser Char st (String, [(String, String)])
    parseInput = (,) <$> template <* endOfLine <* endOfLine <*> insertions
        where template = some upper
              insertions = parseInsertion `endBy1` endOfLine

    parseInsertion :: GenParser Char st (String, String)
    parseInsertion = (,) <$> some upper <* string " -> " <*> some upper

    toList :: (Char, Char) -> String
    toList (a, b) = [a, b]

    apply :: String -> Map String String -> String
    apply template replacements = concatMap (uncurry (:)) $ zip template newInserts
        where newInserts = zipWith (curry ((\x -> findWithDefault "" x replacements) . toList)) template (drop 1 template) ++ [""]

    solve = do
        (template, insertionsList) <- orFail . parse parseInput "Input" =<< getContents
        let insertions = fromList insertionsList
        let final = foldl apply template (replicate 10 insertions)
        let frequencies = map (length &&& head) $ group $ sort final
        let (maxValue, _) = maximum frequencies
        let (minValue, _) = minimum frequencies
        printf "The difference was %d" (maxValue - minValue)
