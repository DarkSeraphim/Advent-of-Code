module Helpers.Parsec (number, numberInteger, parseInput, parseInputWithState, parseString, Parser, StatefulParser, endBy1', sepBy1', spaces) where
    import Text.ParserCombinators.Parsec (GenParser, option, parse, (<|>), try, runParser, many1)
    import Helpers.Input (readInt, orFail)
    import Control.Applicative (some)
    import Text.ParserCombinators.Parsec.Char (digit, char)
    import Control.Monad (void)
    
    type Parser a = StatefulParser () a
    type StatefulParser s a = GenParser Char s a

    number :: StatefulParser a Int
    number = number' readInt

    number' :: Num a => (String -> a) -> StatefulParser s a
    number' parser = do
        sign <- option '+' $ char '-'
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

    parseInput :: StatefulParser () a -> IO a
    parseInput parseFunc = (orFail . parse parseFunc "Input") =<< getContents

    parseInputWithState :: StatefulParser s a -> s -> IO a
    parseInputWithState parseFunc state = (orFail . runParser parseFunc state "input") =<< getContents

    parseString :: StatefulParser () a -> String -> IO a
    parseString parseFunc str = orFail $ parse parseFunc "String" str
