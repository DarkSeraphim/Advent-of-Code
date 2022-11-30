{-# LANGUAGE TupleSections #-}
module Day04.Part2 (solve) where
    import Helpers.Input (readInt, orFail)
    import Text.Parsec ( endBy, parse, (<|>), sepBy1 )
    import Text.ParserCombinators.Parsec (GenParser, many, digit, sepBy, spaces, char, eof, endBy1)
    import Text.Parsec.Char (endOfLine)
    import Text.Printf (printf)
    import Debug.Trace (trace)
    import Control.Applicative (some)
    import Data.Set (Set, fromList, empty, insert, member, difference)
    import Data.List (transpose, find)

    type Board = [[Int]]

    parseInput :: GenParser Char sp ([Int], [Board])
    parseInput = do
        input <- parseInput'
        endOfLine
        endOfLine
        boards <- sepBy1 parseBoard endOfLine
        return (input, boards)

    parseInput' :: GenParser Char sp [Int]
    parseInput' = sepBy1 (readInt <$> many digit) (char ',')

    parseBoard :: GenParser Char sp Board
    parseBoard = endBy1 parseRow endOfLine

    parseRow :: GenParser Char sp [Int]
    parseRow = number `sepBy1` some space

    number :: GenParser Char sp Int
    number = many space *> (readI <$> some digit)

    space = char ' '

    readI x = readInt x'
        where x' = x

    type Possible = (Set Int, Set Int)

    findLast :: Int -> Set Int -> [Possible] -> Set (Set Int) -> Int
    findLast number called sets boards = case match of
        Just board -> number * sum (difference board (insert number called))
        _ -> -1
        -- Find the match where we weren't done before number
        where notit = fromList $ map fst $ filter (bingo called) sets
              match = find (\x -> not $ member x notit) boards
 
    play' :: Set Int -> [Possible] -> Set (Set Int) -> [Int] -> Int
    play' called sets boards [] = 0
    play' called sets boards (number:rest) =
        if match then findLast number called sets boards else play' called' sets boards rest
        where called' = insert number called
              match = fromList (map fst (filter (bingo called') sets)) == boards

    play = play' empty

    -- Compute rows and columns as Sets
    boardToSets :: [[Int]] -> [(Set Int, Set Int)]
    boardToSets board = map (boardSet,) poss
        where poss = map fromList board ++ map fromList (transpose board)
              boardSet = fromList $ concat board

    bingo :: Set Int -> (Set Int, Set Int) -> Bool
    bingo called (board, match) = all (`member` called) match


    solve = do
        (input, boards) <- orFail . parse parseInput "Whoops" =<< getContents
        let sets = concatMap boardToSets boards
        let boards = fromList $ map fst sets
        printf "%d\n" (play sets boards input)

