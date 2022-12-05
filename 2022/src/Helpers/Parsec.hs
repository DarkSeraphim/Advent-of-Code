module Helpers.Parsec (number, parseInput, Parser, endBy1', sepBy1') where
    import Text.ParserCombinators.Parsec (GenParser, option, parse, (<|>), try)
    import Helpers.Input (readInt, orFail)
    import Control.Applicative (some)
    import Text.ParserCombinators.Parsec.Char (digit, char)
    import Text.Parsec (parserTrace)
    import qualified Debug.Trace as Debug
    import Debug.Trace (trace)
    type Parser a = GenParser Char () a

    number :: Parser Int
    number = do
        sign <- option '+' $ char '-'
        let mult = case sign of
                    '+' -> 1
                    '-' -> -1
                    _ -> error "Wat."
        (mult *) . readInt <$> some digit

    endBy1' :: Show a => Parser a -> Parser sep -> Parser [a]
    endBy1' a sep = do
        v <- tryMaybe (a <* sep)
        case v of
            Just v' -> (v' :) <$> endBy1' a sep
            Nothing -> return []

    sepBy1' :: Parser a -> Parser sep -> Parser [a]
    sepBy1' a sep = do
        v <- tryMaybe a
        case v of
            Just v' -> (v' :) <$> (sep *> sepBy1' a sep)
            Nothing -> return []

    tryMaybe :: Parser a -> Parser (Maybe a)
    tryMaybe a = try (Just <$> a) <|> return Nothing

    parseInput :: GenParser Char () a -> IO a
    parseInput parseFunc = (orFail . parse parseFunc "Input") =<< getContents
