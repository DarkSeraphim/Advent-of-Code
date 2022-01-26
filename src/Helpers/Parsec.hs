module Helpers.Parsec (number) where
    import Text.ParserCombinators.Parsec (GenParser, option)
    import Helpers.Input (readInt)
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

