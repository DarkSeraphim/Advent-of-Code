import Data.List (elemIndex, intercalate)
import Helpers.Input (maybeIO)
import System.Directory (listDirectory)
import Debug.Trace

isDay ('D':'a':'y':number) = [number]
isDay _ = []

getPart :: [Char] -> [Char] -> [[Char]]
getPart day ('P':'a':'r':'t':number) = ["Day" ++ day ++ ".Part" ++ number]
getPart _ _ = []

removeExt file =
  let x = elemIndex '.' file in
  case x of
    Just x -> take x file
    Nothing -> file


findDays :: IO [[Char]]
findDays = concatMap isDay <$> listDirectory "src/"
findParts :: [Char] -> IO [[Char]]
findParts day = concatMap (getPart day . removeExt) <$> listDirectory ("src/Day" ++ day)

main = do
  contents <- lines <$> readFile "app/Main.hs"
  start <- maybeIO $ elemIndex "-- AUTOGEN-START" contents
  end <- maybeIO $ elemIndex "-- AUTOGEN-END" contents
  days <- findDays
  parts <- concat <$> mapM findParts days
  putStrLn $ intercalate "\n" (take (start + 1) contents ++ parts ++ drop end contents)
