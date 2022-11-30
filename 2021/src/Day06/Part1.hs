module Day06.Part1 (solve) where
    import Text.Printf (printf)
    import Text.ParserCombinators.Parsec (GenParser, sepBy1, char, eof)
    import Helpers.Parsec (number)
    import Text.ParserCombinators.Parsec.Prim
    import Helpers.Input (orFail)
    import Debug.Trace (trace)
    
    parsePopulation :: GenParser Char st [Int]
    parsePopulation = number `sepBy1` char ',' <* char '\n' <* eof 

    simulate :: Int -> [Int] -> [Int] -> Int
    simulate 0 old young = sum old + sum young
    simulate gen old young = simulate (gen - 1) old' young'
        where old' = tail old ++ [head old + head young]
              young' = tail young ++ [head old]

    solve = do
        population <- (orFail . parse parsePopulation "Whoops") =<< getContents
        let old = map (\gen -> length $ filter (== gen) population) [0..6]
        printf "Population after 80 days: %d" (simulate 80 old [0, 0])
