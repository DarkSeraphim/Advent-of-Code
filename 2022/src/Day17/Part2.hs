module Day17.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (Parser, parseInput)
import Text.Parsec (many1, char, (<|>), endOfLine)
import Data.Functor (($>))
import Lib (newPoint, Point, getY, getX)
import Data.Set (Set, elems, insert, fromList, member)
import Debug.Trace (trace)
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

data Dir = L | R deriving (Ord, Eq)
type Piece = [Point]

parseLine :: Parser [Dir]
parseLine = many1 (L <$ char '<' <|> char '>' $> R) <* endOfLine

pieces :: [Piece]
pieces = map (map (uncurry newPoint)) [
    [(0, 0), (1, 0), (2, 0), (3, 0)], -- -
    [(0, 1), (1, 2), (1, 1), (1, 0), (2, 1)], -- +
    [(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)], -- reverse L
    [(0, 0), (0, 1), (0, 2), (0, 3)], --  |
    [(0, 0), (0, 1), (1, 1), (1, 0)] -- square
  ]

pieceList :: [(Int, Piece)]
pieceList = cycle $ zip [0..] pieces

getHighestY :: Set Point -> Int
getHighestY = maximum . map getY . elems

applyGas :: Point -> [(Int, Dir)] -> (Point, (Int, Dir), [(Int, Dir)])
applyGas _ [] = error "Ran out of gas"
applyGas p (d@(_, L):dir) = (p + newPoint (-1) 0, d, dir)
applyGas p (d@(_, R):dir) = (p + newPoint 1 0, d, dir)

illegal :: Set Point -> Point -> Bool
illegal placements p = outOfBounds p || p `member` placements

outOfBounds :: Point -> Bool
outOfBounds p = x < 0 || x > 6
  where x = getX p

mapT32 :: (b -> b) -> (a, b, c) -> (a, b, c)
mapT32 func (a, b, c) = (a, func b, c)

dropPiece :: Set Point -> Point -> Piece -> [(Int, Dir)] -> (Point, [(Int, Dir)], [(Int, Dir)])
dropPiece placements point piece gas
  | any ((`member` placements) . (point''' +)) piece = (point'', [used], gas')
  | otherwise = mapT32 (used : ) $ dropPiece placements point''' piece gas'
  where (point', used, gas') = applyGas point gas
        -- Correct for pieces moving out of bounds
        point'' = if any (illegal placements . (point' +)) piece then point else point'
        point''' = point'' + newPoint 0 (-1)

placePiece :: Set Point -> [(Int, Piece)] -> [(Int, Dir)] -> Map (Int, Int, Set Point) (Int, Int) -> Int -> Int
placePiece _ [] _ _ _ = error "Pieces should be infinite"
placePiece placements pieces gas seen 0 = trace (printf "%d" (getHighestY placements - 1))$ getHighestY placements - 1
placePiece placements ((pid, piece):pieces') gas seen number
  | seenKey `M.member` seen = trace (printf "Height is %d, repeat from %d, number is %d, repeatNum is %d" newY repeatY number repeatNum) $ skipY + placePiece placements' pieces' gas' M.empty (number - skipNum * (repeatNum - number))
  | otherwise = placePiece placements' pieces' gas' seen'' (number - 1)
  where (res, used, gas') = dropPiece placements (newPoint 2 (getHighestY placements + 4)) piece gas
        placements' = foldl (flip insert) placements $ map (res +) piece
        newY = getHighestY placements'
        seenKey = (pid, fst $ head used, fromList $ map (subtract (newPoint 0 newY)) $ filter (`member` placements') $ concatMap (\y -> map (`newPoint` (newY - y)) [0..6]) [0..40])
        seen' = M.insert seenKey (number, newY) seen
        (repeatNum, repeatY) = M.findWithDefault (0, 0) seenKey seen
        skipNum = number `div` (repeatNum - number)
        skipY = (newY - repeatY) * skipNum
        seen'' = seen'

solve = do
  gas <- cycle . zip [0..] <$> parseInput parseLine
  let floor = fromList $ map (`newPoint` 0) [0..6]
  let result = placePiece floor pieceList gas M.empty 1000000000000
  printf "Highest y is %s\n" (show result)
