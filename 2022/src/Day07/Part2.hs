module Day07.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (string, many1, noneOf, endOfLine, endBy1, (<|>), try)
import Data.Functor (($>))
import Data.Map (Map, empty, insertWith, (!))
import Data.Maybe (catMaybes, mapMaybe)
import Data.List (sort)

data TermLine = Cd String | Ls | LsItem DirEntry deriving Show
data DirEntry = Dir String | File Int String deriving Show
type Path = [String]

root :: Path
root = ["/"]

getDiskSpace :: Int
getDiskSpace = 70000000

getRequired :: Int 
getRequired = 30000000

parseCd :: Parser TermLine
parseCd = Cd <$> (string "cd " *> many1 (noneOf "\n"))

parseLs :: Parser TermLine
parseLs = Ls <$ string "ls"

parseCmd :: Parser TermLine
parseCmd = string "$ " *> (parseCd <|> parseLs)

parseDir :: Parser TermLine
parseDir = LsItem . Dir <$> (string "dir " *> many1 (noneOf "\n"))

parseFile :: Parser TermLine
parseFile = LsItem <$> (File <$> (number <* string " ") <*> many1 (noneOf "\n"))

parse = (parseCmd <|> parseDir <|> parseFile) `endBy1` endOfLine

processTerm :: Path -> [TermLine] -> Map Path [DirEntry]
processTerm _ [] = empty
processTerm path (line:rest) =
  case line of
    Cd name ->
      case name of
        ".." -> processTerm (tail path) rest -- pop
        dirName -> processTerm (dirName:path) rest -- push
    Ls -> processTerm path rest -- NOP
    (LsItem entry) -> insertWith (++) path [entry] (processTerm path rest)

-- | Return the name of the directory, or Nothing if it's a file
getDirectoryName (Dir name) = Just name
getDirectoryName _ = Nothing

-- | Return the size of the file, or Nothing if it's a directory
getFileSize (File size _) = Just size
getFileSize _ = Nothing

-- | Returns the sizes of all directories, as a Pre-order tree walk
-- so root will be the first element, then all children in order of
-- the Map sorting implementation
computeSize :: Map Path [DirEntry] -> Path -> [Int]
computeSize edges start = (sum (map head sizes) + sum fileSizes) : concat sizes
  where x = mapMaybe getDirectoryName (edges ! start)
        sizes = map (\y -> computeSize edges (y : start)) x
        fileSizes = mapMaybe getFileSize (edges ! start)

solve = do
  tree <- processTerm [] <$> parseInput parse
  let sizes = computeSize tree root
  -- the first element in sizes is the root path size, in other words the disk usage
  let toDelete = getRequired - (getDiskSpace - head sizes)
  printf "Deleting %d data" (head $ filter (>= toDelete) (sort sizes))
