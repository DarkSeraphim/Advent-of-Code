module Day01.Part2 (solve) where
  import Text.ParserCombinators.Parsec.Char
  import Text.ParserCombinators.Parsec
  import Helpers.Input (orFail)
  import Text.Printf (printf)

  import Debug.Trace (trace)
  import Data.Set (Set, empty, insert, member, singleton)
  
  movements :: GenParser Char st [(Char, Int)]
  movements = sepBy movement (string ", ")

  movement :: GenParser Char st (Char, Int)
  movement = do
    dir <- char 'R' <|> char 'L'
    mag <- many digit
    let num = read mag
    return (dir, num)

  rot' (0, 1) 'R' = (1, 0)
  rot' (0, 1) 'L' = (-1, 0)
  rot' (1, 0) 'R' = (0, -1)
  rot' (1, 0) 'L' = (0, 1)
  rot' (0, -1) 'R' = (-1, 0)
  rot' (0, -1) 'L' = (1, 0)
  rot' (-1, 0) 'R' = (0, 1)
  rot' (-1, 0) 'L' = (0, -1)
  rot' _ _ = (0, 0)

  walk' :: Set (Int, Int) -> [(Char, Int)] -> (Int, Int) -> (Int, Int) -> Int -> (Int, Int) 
  walk' hist xs pos dir 0 = walk hist (dir, pos) xs
  walk' hist xs (x, y) (dx, dy) n
    | member (x', y') hist = (x', y')
    | otherwise = walk' (insert (x', y') hist) xs (x', y') (dx, dy) (n - 1)
    where x' = x + dx
          y' = y + dy

  walk :: Set (Int, Int) -> ((Int, Int), (Int, Int)) -> [(Char, Int)] -> (Int, Int)
  walk hist (dir, pos) [] = pos
  walk hist (dir, (x, y)) ((rot, mag):xs) = walk' hist xs (x, y) dd mag
    where dd = rot' dir rot 

  solve = do
    contents <- getLine
    moves <- orFail $ parse movements "whoops" contents
    let hist = singleton (0, 0)
    let (x, y) = walk hist ((0, 1), (0, 0)) moves
    printf "You moved %d steps" (abs x + abs y)
