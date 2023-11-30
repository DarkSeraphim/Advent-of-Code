module Helpers.Parsec (number, numberInteger, parseInput, parseString, Parser, endBy1', sepBy1') where
    import Text.ParserCombinators.Parsec (GenParser, option, parse, (<|>), try)
    import Helpers.Input (readInt, orFail)
    import Control.Applicative (some)
    import Text.ParserCombinators.Parsec.Char (digit, char)
    
    type Parser a = GenParser Char () a

    number :: Parser Int
    number = number' readInt

    number' :: Num a => (String -> a) -> Parser a
    number' parser = do
        sign <- option '+' $ char '-'
        let mult = case sign of
                    '+' -> 1
                    '-' -> -1
                    _ -> error "Wat."
        (mult *) . parser <$> some digit

    numberInteger :: Parser Integer
    numberInteger = number' read 

    endBy1' :: Show a => Parser a -> Parser sep -> Parser [a]
    endBy1' a sep = do
        v <- tryMaybe (a <* sep)
        case v of
            Just v' -> (v' :) <$> endBy1' a sep
            Nothing -> return []

    sepBy1' :: Parser a -> Parser sep -> Parser [a]
    sepBy1' a sep = (:) <$> a <*> f
        where f = do
                    v <- tryMaybe sep
                    case v of
                        Just _ -> sepBy1' a sep
                        Nothing -> return []

    tryMaybe :: Parser a -> Parser (Maybe a)
    tryMaybe a = try (Just <$> a) <|> return Nothing

    parseInput :: Parser a -> IO a
    parseInput parseFunc = (orFail . parse parseFunc "Input") =<< getContents

    parseString :: Parser a -> String -> IO a
    parseString parseFunc str = orFail $ parse parseFunc "String" str
