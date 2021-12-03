module Day04.Part2 (solve) where
    import Text.Printf (printf)
    import Text.ParserCombinators.Parsec (letter, endBy, char, many, GenParser, digit, eof, parse)

    import Helpers.Input (readInt, orFail)

    import Text.Parsec.Char (endOfLine)
    import Data.List (group, sort, sortOn, intercalate, isInfixOf)
    import Debug.Trace (trace)
    import Data.Char (isAlpha, ord, chr)
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

    a = ord 'a'

    decrypt :: Int -> Char -> Char
    decrypt key c
      | isAlpha c = chr $ (c' + key') `mod` 26 + a
      | otherwise = c
        where key' = key `mod` 26
              c' = ord c - a


    decryptName :: Room -> String
    decryptName room = map (decrypt i) name'
        where i = id' room
              name' = unwords $ name room


    solve = do
        contents <- getContents
        rooms <- orFail $ parse parseRooms "Whoops" contents
        let validRooms = filter isValid rooms
        -- northpole was mined from output (I printed it to a file and did a few searches :)
        let npRooms = filter (isInfixOf "northpole" . decryptName) validRooms
        putStrLn $ intercalate "\n" $ map (\r -> printf "%d: %s" (id' r) (decryptName r)) npRooms
