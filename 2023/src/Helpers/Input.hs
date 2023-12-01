module Helpers.Input (split, replace, readIntChar, readInt, maybeIO, orFail) where


split :: Char -> String -> [String]
split c (x:xs)
    | x == c = [] : split c xs
    | null xs = [[x]]
    | otherwise = (x : head result) : tail result
        where result = split c xs
split _ [] = []


replace' :: Int -> String -> String -> String -> String
replace' _ _ _ [] = []
replace' nl n r h
  | n == take nl h = r ++ replace n r (drop nl h)
  | otherwise = head h : replace n r (tail h)

replace :: String -> String -> String -> String
replace n = replace' (length n) n

readIntChar :: Char -> Int
readIntChar c = readInt [c]

readInt :: String -> Int
readInt i = read i :: Int

maybeIO :: Maybe a -> IO a
maybeIO (Just x) = return x
maybeIO Nothing = fail "No value"

orFail :: (Show a) => Either a b -> IO b
orFail (Left s) = fail (show s)
orFail (Right b) = return b
