module Day12.Part2 (solve) where
    import Data.Map (Map, insertWith, empty, findWithDefault, insert, singleton, notMember)
    import Text.ParserCombinators.Parsec (string, GenParser, char, noneOf, alphaNum, endBy1, parse)
    import Text.Printf (printf)
    import Control.Applicative (some)
    import Text.Parsec (endOfLine)
    import Helpers.Input (orFail)
    import Data.Set (Set, fromList)
    import Data.Char (isUpper, isLower)
    import Data.List (delete, intercalate)
    
    insertMap = Data.Map.insert

    notMember'  :: String -> Map String Bool -> Bool
    notMember' = findWithDefault True

    parseInput :: GenParser Char st [(String, String)]
    parseInput = endBy1 parseLine endOfLine

    parseLine :: GenParser Char st (String, String)
    parseLine = (,) <$> (some alphaNum <* char '-') <*> some alphaNum

    buildGraph' :: [(String, String)] -> Map String [String] -> Map String [String]
    buildGraph' [] graph = graph
    buildGraph' ((a, b):rest) graph = buildGraph' rest graph''
        where graph' = insertWith (++) a [b] graph
              graph'' = insertWith (++) b [a] graph'

    buildGraph :: [(String, String)] -> Map String [String]
    buildGraph = flip buildGraph' Data.Map.empty

    doStep :: Map String Bool -> Map String [String] -> String -> String -> String -> [[String]]
    doStep visited graph end next special
      | all isUpper next = walkGraph' visited graph end next special
      | next == special && Data.Map.notMember next visited = walkGraph' (insertMap next True visited) graph end next special
      | otherwise = walkGraph' (insertMap next False visited) graph end next special

    walkGraph' :: Map String Bool -> Map String [String] -> String -> String -> String -> [[String]]
    walkGraph' visited graph end next special
      | next == end = [[end]]
      | otherwise = map (next :) $ concatMap (\x -> doStep visited graph end x special) possible
        where neighbours = findWithDefault [] next graph
              possible = filter (`notMember'` visited) neighbours

    walkGraph :: Map String [String] -> String -> String -> String -> [[String]]
    walkGraph graph end start = walkGraph' (singleton start False) graph end start

    toList :: (String, String) -> [String]
    toList (a, b) = [a, b]

    solve = do
        edges <- (orFail . parse parseInput "Input") =<< getContents
        let graph = buildGraph edges
        let lower = delete "end" $ delete "start" $ filter (all isLower) (concatMap toList edges)
        let paths = concatMap (walkGraph graph "end" "start") lower
        let uniq = fromList paths
        -- printf "%s\n" $ show uniq
        printf "There are %d unique paths" (length uniq)

