module Day02.Part1 (solve) where
import Text.Printf (printf)
import qualified Data.Map
import Text.Parsec.Char (oneOf, endOfLine)
import Helpers.Parsec (parseInput)
import Text.Parsec (endBy1)
import Data.Map ((!))

data Move = Rock | Paper | Scissors deriving (Enum, Eq, Ord)
data Result = Lose | Tie | Win

encoderLeft = Data.Map.fromList [('A', Rock), ('B', Paper), ('C', Scissors)]
encoderRight = Data.Map.fromList [('X', Rock), ('Y', Paper), ('Z', Scissors)]

leftMove = oneOf "ABC"
rightMove = oneOf "XYZ"
convertMoves a b = (encoderLeft ! a, encoderRight ! b)
line = convertMoves <$> (leftMove <* oneOf " ") <*> rightMove
parse = line `endBy1` endOfLine

scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

toResult Rock Rock         = Tie
toResult Rock Paper        = Win
toResult Rock Scissors     = Lose
toResult Paper Rock        = Lose
toResult Paper Paper       = Tie
toResult Paper Scissors    = Win
toResult Scissors Rock     = Win
toResult Scissors Paper    = Lose
toResult Scissors Scissors = Tie

scoreResult Lose = 0
scoreResult Tie  = 3
scoreResult Win  = 6

score :: (Move, Move) -> Int
score (l, r) = scoreMove r + scoreResult (toResult l r)

solve = do
  s <- sum . map score <$> parseInput parse
  printf "The score is %d" s
