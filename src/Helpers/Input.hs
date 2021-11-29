module Helpers.Input (split, readInteger, maybeIO) where


split :: Char -> String -> [String]
split c (x:xs)
    | x == c = [] : split c xs
    | null xs = [[x]]
    | otherwise = (x : head result) : tail result
        where result = split c xs
split c [] = []

readInteger :: String -> Integer
readInteger = read

maybeIO :: Maybe a -> IO a
maybeIO (Just x) = return x 
maybeIO Nothing = fail "No value"
