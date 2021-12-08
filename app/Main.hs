module Main where

import Text.Printf
import Lib
import System.Environment (getArgs)

-- Generate solve implementations by script, as Haskell doesn't have a "proper"
-- way to generate imports?
-- AUTOGEN-START
import Day02.Part1
import Day02.Part2
import Day08.Part1
import Day07.Part1
import Day07.Part2
import Day03.Part1
import Day03.Part2
import Day05.Part1
import Day05.Part2
import Day01.Part1
import Day01.Part2
import Day06.Part1
import Day06.Part2
import Day04.Part1
import Day04.Part2
solveDay "2" "1" = Day02.Part1.solve
solveDay "2" "2" = Day02.Part2.solve
solveDay "8" "1" = Day08.Part1.solve
solveDay "7" "1" = Day07.Part1.solve
solveDay "7" "2" = Day07.Part2.solve
solveDay "3" "1" = Day03.Part1.solve
solveDay "3" "2" = Day03.Part2.solve
solveDay "5" "1" = Day05.Part1.solve
solveDay "5" "2" = Day05.Part2.solve
solveDay "1" "1" = Day01.Part1.solve
solveDay "1" "2" = Day01.Part2.solve
solveDay "6" "1" = Day06.Part1.solve
solveDay "6" "2" = Day06.Part2.solve
solveDay "4" "1" = Day04.Part1.solve
solveDay "4" "2" = Day04.Part2.solve
-- AUTOGEN-END

-- solve :: String -> String -> IO ()
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