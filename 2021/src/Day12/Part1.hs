module Day12.Part1 (solve) where
    import Data.Map (Map, insertWith, empty, findWithDefault)
    import Text.ParserCombinators.Parsec (string, GenParser, char, noneOf, alphaNum, endBy1, parse)
    import Text.Printf (printf)
    import Control.Applicative (some, Alternative (empty))
    import Text.Parsec (endOfLine)
    import Helpers.Input (orFail)

    import Data.Set (Set, empty, singleton, notMember, insert, union, unions, fromList)
    import Data.Char (isUpper)
    
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

    doStep :: Set String -> Map String [String] -> String -> String -> [[String]]
    doStep visited graph end next
      | all isUpper next = walkGraph' visited graph end next
      | otherwise = walkGraph' (insert next visited) graph end next

    walkGraph' :: Set String -> Map String [String] -> String -> String -> [[String]]
    walkGraph' visited graph end next 
      | next == end = [[end]]
      | otherwise = map (next :) $ concatMap (doStep visited graph end) possible
        where neighbours = findWithDefault [] next graph
              possible = filter (`notMember` visited) neighbours

    walkGraph :: Map String [String] -> String -> String -> [[String]]
    walkGraph graph end start = walkGraph' (singleton start) graph end start

    solve = do
        edges <- (orFail . parse parseInput "Input") =<< getContents
        let graph = buildGraph edges
        let paths = walkGraph graph "end" "start"
        let uniq = fromList paths
        printf "There are %d unique paths" (length uniq)
        
