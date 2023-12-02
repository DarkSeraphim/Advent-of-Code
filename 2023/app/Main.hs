module Main (main) where

import Text.Printf
import System.Environment (getArgs)

-- Generate solve implementations by script, as Haskell doesn't have a "proper"
-- way to generate imports?
-- AUTOGEN-START
import Day01.Part2
import Day01.Part1
import Day02.Part2
import Day02.Part1
solveDay :: String -> String -> IO ()
solveDay "1" "2" = Day01.Part2.solve
solveDay "1" "1" = Day01.Part1.solve
solveDay "2" "2" = Day02.Part2.solve
solveDay "2" "1" = Day02.Part1.solve
-- AUTOGEN-END

-- solveDay :: String -> String -> IO ()
solveDay d p = printf "Day '%s' or part '%s' of day not found" d p

getDay :: [String] -> String
getDay (x:_) = x
getDay _ = ""

getPart :: [String] -> String
getPart (_:x:_) = x
getPart _ = ""

main :: IO ()
main = do
  args <- getArgs
  solveDay (getDay args) (getPart args)