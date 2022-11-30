module Helpers.Parsec (number, parseInput) where
    import Text.ParserCombinators.Parsec (GenParser, option, parse)
    import Helpers.Input (readInt, orFail)
    import Control.Applicative (some)
    import Text.ParserCombinators.Parsec.Char (digit, char)

    number :: GenParser Char st Int
    number = do
        sign <- option '+' $ char '-'
        let mult = case sign of
                    '+' -> 1
                    '-' -> -1
                    _ -> error "Wat."
        (mult *) . readInt <$> some digit

    parseInput :: GenParser Char () a -> IO a
    parseInput parseFunc = (orFail . parse parseFunc "Input") =<< getContents
