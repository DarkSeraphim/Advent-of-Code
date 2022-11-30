module Day14.Part3 (solve) where
    import Text.ParserCombinators.Parsec (GenParser, upper, endBy1, string, parse)
    import Text.Printf (printf)
    import Text.Parsec.Char (endOfLine)
    import Control.Applicative (some)
    import Helpers.Input (orFail)
    import Data.Map (fromList, Map, lookup, empty, unionsWith, toList)

    type Node = (Char, Char)

    lookupMap :: Ord k => k -> Map k v -> Maybe v
    lookupMap = Data.Map.lookup

    parseInput :: GenParser Char st (String, [(Node, Char)])
    parseInput = (,) <$> template <* endOfLine <* endOfLine <*> insertions
        where template = some upper
              insertions = parseInsertion `endBy1` endOfLine

    parseInsertion :: GenParser Char st (Node, Char)
    parseInsertion = (,) <$> ((,) <$> upper <*> upper) <* string " -> " <*> upper

    getNeighbours :: (Char, Char) -> Map Node Char -> Maybe [Node]
    getNeighbours cur inserts = do
        insert <- lookupMap cur inserts
        let (a, b) = cur
        pure [(a, insert), (insert, b)]

    dfs :: Int -> Map Node Char -> Node -> Map Char Int
    dfs depth inserts cur = case n of
        Just ns -> unionsWith (+) $ (self :) $ map (dfs (depth - 1) inserts) ns
        Nothing -> self
        where n = if depth == 0 then Nothing else getNeighbours cur inserts
              self = fromList [(a, 1), (b, 1)]
              (a, b) = cur

    solve = do
        (template, insertionsList) <- (orFail . parse parseInput "Input") =<< getContents
        let insertions = fromList insertionsList
        let roots = zip template (drop 1 template)
        let final = unionsWith (+) $ map (dfs 10 insertions) roots

        let frequencies = map snd $ toList final
        let maxValue = maximum frequencies
        let minValue = minimum frequencies


        printf "Answer: %d" ((maxValue `div` 3) - (minValue `div` 3))
