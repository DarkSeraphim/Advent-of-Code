module Main (main) where

import Text.Printf
import System.Environment (getArgs)

-- Generate solve implementations by script, as Haskell doesn't have a "proper"
-- way to generate imports?
-- AUTOGEN-START
import Day15.Part1
import Day15.Part2
import Day16.Part1
import Day16.Part2
import Day17.Part1
import Day17.Part2
import Day18.Part1
import Day18.Part2
solveDay :: String -> String -> IO ()
solveDay "15" "1" = Day15.Part1.solve
solveDay "15" "2" = Day15.Part2.solve
solveDay "16" "1" = Day16.Part1.solve
solveDay "16" "2" = Day16.Part2.solve
solveDay "17" "1" = Day17.Part1.solve
solveDay "17" "2" = Day17.Part2.solve
solveDay "18" "1" = Day18.Part1.solve
solveDay "18" "2" = Day18.Part2.solve
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