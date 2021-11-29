module Main where

import Lib
import System.Environment (getArgs)

solve :: String -> String -> IO ()
solve _ _ = putStrLn "Day not found"

-- Generate solve implementations by script, as Haskell doesn't have a "proper"
-- way to generate imports?
-- AUTOGEN-START
-- AUTOGEN-END

getDay :: [String] -> String
getDay (x:_) = x
getDay _ = ""

getPart :: [String] -> String
getPart (_:x:_) = x
getPart _ = ""

main :: IO ()
main = do
  args <- getArgs
  solve (getDay args) (getPart args)
