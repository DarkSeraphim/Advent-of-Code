import Data.List (elemIndex, intercalate)
import Helpers.Input (maybeIO, readInt)
import System.Directory (listDirectory)
import Text.Printf (printf)

isDay :: String -> [String]
isDay ('D':'a':'y':number) = [number]
isDay _ = []

getPart :: [Char] -> [Char] -> [(String, String, [Char])]
getPart day ('P':'a':'r':'t':number) = [(day, number, "Day" ++ day ++ ".Part" ++ number)]
getPart _ _ = []

removeExt :: String -> String
removeExt file =
  let dotIdx = elemIndex '.' file in
  case dotIdx of
    Just idx -> take idx file
    Nothing -> file


findDays :: IO [[Char]]
findDays = concatMap isDay <$> listDirectory "src/"
findParts :: [Char] -> IO [(String, String, [Char])]
findParts day = concatMap (getPart day . removeExt) <$> listDirectory ("src/Day" ++ day)

buildImport :: String -> String
buildImport mod' = "import " ++ mod'

buildSolve :: (String, String, String) -> String
buildSolve (day, part, mod') = printf "solveDay \"%s\" \"%s\" = %s.solve" d part mod'
  where d = show (readInt day)

getMod :: (String, String, String) -> String
getMod (_, _, mod') = mod'

main :: IO ()
main = do
  contents <- lines <$> readFile "app/Main.hs.template"
  start <- maybeIO $ elemIndex "-- AUTOGEN-START" contents
  end <- maybeIO $ elemIndex "-- AUTOGEN-END" contents
  days <- findDays
  parts <- concat <$> mapM findParts days
  let imports = map (buildImport . getMod) parts
  let solves = "solveDay :: String -> String -> IO ()" : map buildSolve parts
  writeFile "app/Main.hs" $ intercalate "\n" (take (start + 1) contents ++ imports ++ solves ++ drop end contents)
