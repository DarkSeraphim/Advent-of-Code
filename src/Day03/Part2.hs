module Day03.Part2 (solve) where
    import Text.ParserCombinators.Parsec (sepBy, many, digit, spaces, GenParser, parse, eof, endBy)
    import Helpers.Input (readInt, orFail)
    import Text.Parsec.Char (endOfLine)
    import Text.Printf (printf)
    import Debug.Trace (trace)
    import Data.List (transpose)
    type Triangle = (Int, Int, Int)
   
    triangles :: GenParser Char st [Triangle]
    triangles = do
        t <- endBy triangle endOfLine
        eof
        return t
    
    triangle :: GenParser Char st Triangle
    triangle = do
        spaces
        a <- number
        spaces 
        b <- number
        spaces
        c <- number
        return (a, b, c) 

    number :: GenParser Char st Int
    number = readInt <$> many digit

    isPossible (a, b, c) = aok && bok && cok
        where aok = a < b + c
              bok = b < a + c
              cok = c < a + b

    toList :: Triangle -> [Int]
    toList (a, b, c) = [a, b, c]

    count :: [Int] -> IO Int
    count [] = return 0 
    count (a:b:c:rest)
      | isPossible (a, b, c) = (+1) <$> count rest
      | otherwise = count rest
    count _ = fail "Invalid triangle count"

    solve = do
        contents <- getContents
        triangles <- orFail $ parse triangles "Whoops" contents
        let ts = concat $ transpose $ map toList triangles
        n <- count ts
        printf "Found %d valid triangles" n
