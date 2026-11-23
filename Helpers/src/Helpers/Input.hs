{-# LANGUAGE TupleSections #-}
module Helpers.Input (split, replace, readIntChar, readInt, maybeIO, orFail, toPointMap, toPointMapMaybe, toPointMapMaybeWithKey) where
import Helpers.Point (Point, newPoint)
import Data.Map (Map, fromList)
import Data.Maybe (catMaybes)
import Helpers.List (tail', head')
import Data.List (uncons)

split :: Eq a => a -> [a] -> [[a]]
split c (x:xs)
    | x == c = [] : split c xs
    | null xs = [[x]]
      -- We know that result is never empty, otherwise the previous case would be hit
    | otherwise = (x : head' result) : tail' result
        where result = split c xs
split _ [] = []


replace :: String -> String -> String -> String
replace needle replacement haystack
  | needle == take nl haystack = replacement ++ replace needle replacement (drop nl haystack)
  | otherwise = case uncons haystack of
                  Just (h', t') -> h' : replace needle replacement t'
                  Nothing -> []
  where nl = length needle

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

toPointMapMaybe :: (a -> Maybe b) -> [[a]] -> Map Point b
toPointMapMaybe f = toPointMapMaybeWithKey (\_ a -> f a)
toPointMapMaybeWithKey :: (Point -> a -> Maybe b) -> [[a]] -> Map Point b
toPointMapMaybeWithKey f d = fromList $ catMaybes [(newPoint x y,) <$>  f (newPoint x y) ((d !! y) !! x) | y <- axis, x <- [0 .. length (d !! y) - 1]]
  where axis = [0 .. (length d - 1)]

toPointMap :: (a -> b) -> [[a]] -> Map Point b
toPointMap f = toPointMapMaybe (Just . f)
