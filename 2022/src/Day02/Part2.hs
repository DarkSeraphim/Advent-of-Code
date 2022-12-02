module Day02.Part2 (solve) where
import Text.Printf (printf)
import qualified Data.Map
import Text.Parsec.Char (oneOf, endOfLine)
import Helpers.Parsec (parseInput)
import Text.Parsec (endBy1)
import Data.Map ((!))

data Move = Rock | Paper | Scissors deriving (Enum, Eq, Ord)
data Result = Lose | Tie | Win

encoderLeft = Data.Map.fromList [('A', Rock), ('B', Paper), ('C', Scissors)]
encoderRight = Data.Map.fromList [('X', Lose), ('Y', Tie), ('Z', Win)]

leftMove = oneOf "ABC"
rightMove = oneOf "XYZ"
convertMoves a b = (encoderLeft ! a, encoderRight ! b)
line = convertMoves <$> (leftMove <* oneOf " ") <*> rightMove
parse = line `endBy1` endOfLine

scoreMove Rock = 1
scoreMove Paper = 2
scoreMove Scissors = 3

toMove Rock Tie = Rock
toMove Rock Win = Paper
toMove Rock Lose = Scissors
toMove Paper Lose = Rock
toMove Paper Tie = Paper
toMove Paper Win = Scissors
toMove Scissors Win = Rock
toMove Scissors Lose = Paper
toMove Scissors Tie = Scissors



scoreResult Lose = 0
scoreResult Tie  = 3
scoreResult Win  = 6

score :: (Move, Result) -> Int
score (l, r) = scoreMove (toMove l r) + scoreResult r

solve = do
  s <- sum . map score <$> parseInput parse
  printf "The score is %d" s
