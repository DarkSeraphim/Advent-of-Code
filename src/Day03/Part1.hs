module Day03.Part1 (solve) where
    import Text.ParserCombinators.Parsec (sepBy, many, digit, spaces, GenParser, parse, eof, endBy)
    import Helpers.Input (readInt, orFail)
    import Text.Parsec.Char (endOfLine)
    import Text.Printf (printf)
    import Debug.Trace (trace)
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

    solve = do
        contents <- getContents 
        triangles <- orFail $ parse triangles "Whoops" contents 
        printf "Found %d valid triangles" (length $ filter isPossible triangles)
