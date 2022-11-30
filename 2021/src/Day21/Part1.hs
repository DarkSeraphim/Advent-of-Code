module Day21.Part1 (solve) where
    import Text.Parsec (endOfLine)
    import Text.ParserCombinators.Parsec (GenParser, eof, string, anyChar)
    import Helpers.Parsec (number, parseInput)
    import Text.Printf (printf)
    import Debug.Trace (trace)
    
    data Player = Player {score :: Int, pos :: Int}

    parsePlayers :: GenParser Char st (Player, Player)
    parsePlayers = (,) <$> (parsePlayer <* endOfLine) <*> (parsePlayer <* endOfLine) <* eof

    -- Put the position 1 back, so it fits in mod 10
    parsePlayer :: GenParser Char st Player
    parsePlayer = Player 0 <$> (string "Player " *> anyChar *> string " starting position: " *> ((\x -> x - 1) <$> number))

    rollDice' :: (Player, Player) -> [Int] -> Int -> Int
    rollDice' (playerA, playerB) (a:b:c:dice) roll
      | score newA >= 1000 = trace (printf "%d - %d" (score playerB) roll) $ score playerB * (roll * 3)
      | otherwise          = rollDice' (playerB, newA) dice (roll + 1)
      where pos' = (a + b + c + pos playerA) `mod` 10
            newA = Player (score playerA + pos' + 1) pos'
    rollDice' _ _ _ = error "Magic dice broke"

    rollDice :: (Player, Player) -> [Int] -> Int
    rollDice players dice = rollDice' players dice 1

    solve = do        
        players <- parseInput parsePlayers
        let dice = cycle [1..100] 
        printf "Final score of losing player, multiplied by rolls is %d\n" (rollDice players dice)
