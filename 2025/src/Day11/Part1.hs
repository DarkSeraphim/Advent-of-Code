module Day11.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec
import Text.Parsec
import Data.Map (Map, fromList, empty, insert, findWithDefault, (!))

nodeP :: Parser String
nodeP = many1 (noneOf " :\n\r")

edgesP :: Parser (String, [String])
edgesP = (,) <$> (nodeP <* string ": ") <*> (nodeP `sepBy1` char ' ')

graphP :: Parser [(String, [String])]
graphP = edgesP `endBy1` endOfLine

walk :: Map String [String] -> Map String Int -> String -> Map String Int
walk edges memory current
  | current == "out" = insert current 1 memory
  | otherwise = insert current currentResult memory'
  where next = findWithDefault [] current edges
        memory' = foldl (walk edges) memory next
        currentResult = sum $ map (memory' !) next

solve :: IO ()
solve = do
  graph <- fromList <$> parseInput graphP
  let res = walk graph empty "you" ! "you"
  printf "There are %d paths to out\n" res
