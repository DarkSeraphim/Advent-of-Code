module Day11.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec
import Text.Parsec
import Data.Map (Map, fromList, empty, insert, findWithDefault, (!), delete, member)

nodeP :: Parser String
nodeP = many1 (noneOf " :\n\r")

edgesP :: Parser (String, [String])
edgesP = (,) <$> (nodeP <* string ": ") <*> (nodeP `sepBy1` char ' ')

graphP :: Parser [(String, [String])]
graphP = edgesP `endBy1` endOfLine

walk :: Map String [String] -> String -> Map String Int -> String -> Map String Int
walk edges end memory current
  | current == end = insert current 1 memory
  | current `member` memory = memory
  | otherwise = insert current currentResult memory'
  where next = findWithDefault [] current edges
        memory' = foldl (walk edges end) memory next
        currentResult = sum $ map (memory' !) next

withoutNodes :: Map String [String] -> [String] -> Map String [String]
withoutNodes = foldl (flip delete)

solve :: IO ()
solve = do
  graph <- fromList <$> parseInput graphP
  let svr2dac = walk (withoutNodes graph ["fft", "out"]) "dac" empty "svr" ! "svr"
  let svr2fft = walk (withoutNodes graph ["dac", "out"]) "fft" empty "svr" ! "svr"
  let dac2fft = walk (withoutNodes graph ["svr", "out"]) "fft" empty "dac" ! "dac"
  let fft2dac = walk (withoutNodes graph ["svr", "out"]) "dac" empty "fft" ! "fft"
  let fft2out = walk (withoutNodes graph ["dac", "svr"]) "out" empty "fft" ! "fft"
  let dac2out = walk (withoutNodes graph ["fft", "svr"]) "out" empty "dac" ! "dac"
  
  printf "Values: %s" (show [svr2dac, svr2fft, dac2fft, fft2dac, fft2out, dac2out])
  let res = (svr2dac * dac2fft *fft2out) + (svr2fft * fft2dac * dac2out)
  printf "There are %d paths to out\n" res
