{-# LANGUAGE TupleSections #-}
module Day13.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (endOfLine, many1)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec.Char (noneOf)
import Text.Parsec.Combinator (sepEndBy1)
import Data.List (transpose, partition)
import Data.Maybe (fromJust)
import Control.Applicative ((<|>))

data Orientation = Horizontal | Vertical deriving (Eq)
type Block = [String]

pLines :: Parser Block
pLines = many1 (noneOf "\r\n") `sepEndBy1` endOfLine

pBlocks :: Parser [Block]
pBlocks = pLines `sepEndBy1` endOfLine

countSmudges' :: String -> String -> Int
countSmudges' a b = length $ filter (uncurry (/=)) $ zip a b

countSmudges :: [String] -> [String] -> Int
countSmudges left right = sum $ zipWith countSmudges' (reverse left) right

findReflection :: Int -> Block -> Maybe Int
findReflection i block
  | i >= length block = Nothing
  | 1 == countSmudges (take i block) (drop i block) = Just i
  | otherwise = findReflection (i + 1) block


findReflectionAndOrientation :: Block -> (Orientation, Int)
findReflectionAndOrientation block = fromJust $ (((Horizontal, ) <$> hor) <|> ((Vertical, ) <$> ver))
  where hor = findReflection 1 (transpose block)
        ver = findReflection 1 block


solve :: IO ()
solve = do
  blocks <- parseInput pBlocks
  let reflectionPoints = map findReflectionAndOrientation blocks
  let (hor, ver) = partition ((== Horizontal) . fst) reflectionPoints
  printf "The score was %d\n" (100 * sum (map snd ver) + sum ( map snd hor))
