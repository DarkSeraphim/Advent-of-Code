module Day21.Part2 (solve) where
    import Text.Parsec (endOfLine)
    import Text.ParserCombinators.Parsec (GenParser, eof, string, anyChar)
    import Helpers.Parsec (number, parseInput)
    import Text.Printf (printf)
    import Data.Map (Map, empty, insert, member, (!))

    import Data.List (sort, group)
    import qualified Control.Arrow as Data.Bifunctor
    data Player = Player {score :: Int, pos :: Int}

    instance Eq Player where
        a == b = (score a, pos a) == (score b, pos b)
    instance Ord Player where
        a <= b = (score a, pos a) <= (score b, pos b)

    parsePlayers :: GenParser Char st (Player, Player)
    parsePlayers = (,) <$> (parsePlayer <* endOfLine) <*> (parsePlayer <* endOfLine) <* eof

    -- Put the position 1 back, so it fits in mod 10
    parsePlayer :: GenParser Char st Player
    parsePlayer = Player 0 <$> (string "Player " *> anyChar *> string " starting position: " *> ((\x -> x - 1) <$> number))

    type Wins = (Int, Int)
    -- Roll value with frequency in outcome list
    type Roll = (Int, Int)
    type Memory = Map (Player, Player) Wins

    quantumDiceFreq :: [(Int, Int)]
    quantumDiceFreq = freq
      where rolls = concatMap (\b -> concatMap (\a -> map ((a + b) +) [1..3]) [1..3]) [1..3]
            sorted = group $ sort rolls
            freq = map (\v -> (head v, length v)) sorted

    addWins :: Wins -> Wins -> Wins
    addWins (a, b) (c, d) = (a + c, b + d)

    scaleWins :: Wins -> Int -> Wins
    scaleWins (a, b) c = (a * c, b * c)

    flipWins :: Wins -> Wins
    flipWins (a, b) = (b, a)

    rollQuantum :: (Player, Player) -> (Memory, Wins) -> Roll -> (Memory, Wins)
    rollQuantum ps@(playerA, playerB) (memory, wins) (roll, freq) = (memory'', addWins wins swins)
      where pos' = (roll + pos playerA) `mod` 10
            newA = Player {score = score playerA + pos' + 1, pos = pos'}
            (memory', wins') = Data.Bifunctor.second flipWins $ rollDice' memory (playerB, newA)
            swins = scaleWins wins' freq
            memory'' = insert ps swins memory'

    rollDice' :: Memory -> (Player, Player) -> (Memory, Wins)
    rollDice' memory ps@(playerA, playerB)
      | score playerB >= 21 = (memory, (0, 1))
      | member (playerA, playerB) memory = (memory, memory ! (playerA, playerB))
      | otherwise           = (memory'', wins)
      where (memory', wins) = foldl (rollQuantum ps) (memory, (0, 0)) quantumDiceFreq
            memory'' = insert ps wins memory'
            --wins = flipWins $ addWins wins1 wins2 wins3
            --memory' = insert (newA3, playerB) wins memory3

    rollDice :: (Player, Player) -> (Int, Int)
    rollDice players = snd $ rollDice' empty players

    best :: (Int, Int) -> Int
    best (a, b)
      | a > b     = a
      | otherwise = b

    solve = do
        players <- parseInput parsePlayers
        let wins = rollDice players
        printf "Final number of wins %s. The best player scored %d\n" (show wins) (best wins)
