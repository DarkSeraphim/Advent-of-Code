module Day04.Part1 (solve) where
    import Text.Printf (printf)
    import Text.ParserCombinators.Parsec (letter, endBy, char, many, GenParser, digit, eof, parse)

    import Helpers.Input (readInt, orFail)

    import Text.Parsec.Char (endOfLine)
    import Data.List (group, sort, sortOn, intercalate)
    import Debug.Trace (trace)
    data Room = Room { name :: [String], id' :: Int, checksum :: String }

    parseRooms :: GenParser Char st [Room]
    parseRooms = endBy parseRoom endOfLine <* eof

    parseRoom :: GenParser Char st Room
    parseRoom = do
        name <- parseName
        id <- readInt <$> many digit
        checksum <- char '[' *> many letter <* char ']'
        return Room { name = name, id' = id, checksum = checksum}

    parseName :: GenParser Char st [String]
    parseName = many letter `endBy` char '-'

    isValid :: Room -> Bool
    isValid room = result == checksum room
        where groups = group $ sort $ concat $ name room

              groups' = take 5 $ sortOn (\group -> (- length group, head group)) groups
              result = map head groups'

    solve = do
        contents <- getContents
        rooms <- orFail $ parse parseRooms "Whoops" contents
        let sol = sum $ map id' $ filter isValid rooms
        printf "The sum of ids is %d\n" sol
