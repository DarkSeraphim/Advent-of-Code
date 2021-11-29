import Data.List (elemIndex, intercalate)
import Helpers.Input (maybeIO)
import System.Directory (listDirectory)
import Debug.Trace

isDay ('D':'a':'y':number) = [number]
isDay _ = []

getPart :: [Char] -> [Char] -> [(String, String, [Char])]
getPart day ('P':'a':'r':'t':number) = [(day, number, "Day" ++ day ++ ".Part" ++ number)]
getPart _ _ = []

removeExt file =
  let x = elemIndex '.' file in
  case x of
    Just x -> take x file
    Nothing -> file


findDays :: IO [[Char]]
findDays = concatMap isDay <$> listDirectory "src/"
findParts :: [Char] -> IO [(String, String, [Char])]
findParts day = concatMap (getPart day . removeExt) <$> listDirectory ("src/Day" ++ day)

buildImport day = "import Day" ++ day
buildSolve (day, part, mod) = "solve " ++ "\"" ++ day ++ "\" " ++ "\"" ++ part ++ "\" = " ++ mod ++ ".solve"

main = do
  contents <- lines <$> readFile "app/Main.hs"
  start <- maybeIO $ elemIndex "-- AUTOGEN-START" contents
  end <- maybeIO $ elemIndex "-- AUTOGEN-END" contents
  days <- findDays
  parts <- concat <$> mapM findParts days
  let imports = map buildImport days
  let solves = map buildSolve parts
  putStrLn $ intercalate "\n" (take (start + 1) contents ++ imports ++ solves ++ drop end contents)
