{-# LANGUAGE TupleSections #-}
module Helpers.Parsec (
  number, 
  numberInteger,
  parseInput,
  parseInputWithState,
  parseGrid,
  parseGridMaybe,
  parseString,
  parseFile,
  Parser,
  StatefulParser,
  endBy1',
  sepBy1',
  spaces,
  point2D,
  point3D) where
import Text.ParserCombinators.Parsec (GenParser, option, parse, (<|>), try, runParser, many1, SourcePos, getPosition, sourceColumn)
import Helpers.Input (readInt, orFail)
import Control.Applicative (some)
import Text.ParserCombinators.Parsec.Char (digit, char)
import Control.Monad (void)
import Text.Parsec.Pos (initialPos, sourceLine)
import Helpers.Point (Point, newPoint, newPoint3)
import Data.Map (Map, fromList)
import Text.Parsec (noneOf, endOfLine, sepEndBy1, oneOf)
import Data.Maybe (catMaybes)
import System.IO (IOMode(ReadMode), openFile, hGetContents)

type Parser a = StatefulParser () a
type StatefulParser s a = GenParser Char s a

number :: StatefulParser a Int
number = number' readInt

number' :: Num a => (String -> a) -> StatefulParser s a
number' parser = do
    sign <- option '+' $ oneOf "-+"
    let mult = case sign of
                '+' -> 1
                '-' -> -1
                _ -> error "Wat."
    (mult *) . parser <$> some digit

numberInteger :: StatefulParser s Integer
numberInteger = number' read

spaces :: StatefulParser a ()
spaces = void (many1 (char ' '))

endBy1' :: Show a => StatefulParser s a -> StatefulParser s sep -> StatefulParser s [a]
endBy1' a sep = do
    v <- tryMaybe (a <* sep)
    case v of
        Just v' -> (v' :) <$> endBy1' a sep
        Nothing -> return []

sepBy1' :: StatefulParser s a -> StatefulParser s sep -> StatefulParser s [a]
sepBy1' a sep = (:) <$> a <*> f
    where f = do
                v <- tryMaybe sep
                case v of
                    Just _ -> sepBy1' a sep
                    Nothing -> return []

tryMaybe :: StatefulParser s a -> StatefulParser s (Maybe a)
tryMaybe a = try (Just <$> a) <|> return Nothing

point2D :: Parser Point
point2D = newPoint <$> (number <* char ',') <*> number

point3D :: Parser Point 
point3D = newPoint3 <$> (number <* char ',') <*> (number <* char ',') <*> number

parseInput :: StatefulParser () a -> IO a
parseInput parseFunc = (orFail . parse parseFunc "Input") =<< getContents

parseInputWithState :: StatefulParser s a -> s -> IO a
parseInputWithState parseFunc state = (orFail . runParser parseFunc state "input") =<< getContents

parsePoint :: (Char -> Maybe a) -> StatefulParser SourcePos (Maybe (Point, a))
parsePoint func = do
    pos <- getPosition
    fmap (newPoint (sourceColumn pos) (sourceLine pos), ) . func <$> noneOf "\r\n"

parsePoints :: (Char -> Maybe a) -> StatefulParser SourcePos [(Point, a)]
parsePoints f = catMaybes . concat <$> many1 (parsePoint f) `sepEndBy1` endOfLine

parseGrid :: (Char ->  a) -> IO (Map Point a)
parseGrid f = parseGridMaybe (return . f)

parseGridMaybe :: (Char -> Maybe a) -> IO (Map Point a)
parseGridMaybe f = fromList <$> (`parseInputWithState` initialPos "input") (parsePoints f)

parseString :: StatefulParser () a -> String -> IO a
parseString parseFunc str = orFail $ parse parseFunc "String" str

parseFile :: StatefulParser () a -> String -> IO a
parseFile parseFunc file = do
    handle <- openFile file ReadMode
    contents <- hGetContents handle
    parseString parseFunc contents
