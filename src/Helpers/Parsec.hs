module Helpers.Parsec (number) where
    import Text.ParserCombinators.Parsec (GenParser)
    import Helpers.Input (readInt)
    import Control.Applicative (some)
    import Text.ParserCombinators.Parsec.Char (digit)
    
    number :: GenParser Char st Int
    number = readInt <$> some digit

