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

data Dir = L | R
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

pieceList :: [Piece]
pieceList = cycle pieces

getHighestY :: Set Point -> Int
getHighestY = maximum . map getY . elems

applyGas :: Point -> [Dir] -> (Point, [Dir])
applyGas _ [] = error "Ran out of gas"
applyGas p (L:dir) = (p + newPoint (-1) 0, dir)
applyGas p (R:dir) = (p + newPoint 1 0, dir)

illegal :: Set Point -> Point -> Bool
illegal placements p = outOfBounds p || p `member` placements

outOfBounds :: Point -> Bool
outOfBounds p = x < 0 || x > 6
  where x = getX p

dropPiece :: Set Point -> Point -> Piece -> [Dir] -> (Point, [Dir])
dropPiece placements point piece gas
  | any ((`member` placements) . (point''' +)) piece = (point'', gas')
  | otherwise = dropPiece placements point''' piece gas'
  where (point', gas') = applyGas point gas
        -- Correct for pieces moving out of bounds
        point'' = if any (illegal placements . (point' +)) piece then point else point'
        point''' = point'' + newPoint 0 (-1)

placePiece :: Set Point -> [Piece] -> [Dir] -> Integer -> [Int]
placePiece _ [] _ _ = error "Pieces should be infinite"
placePiece placements pieces gas 0 = [getHighestY placements]
placePiece placements (piece:pieces') gas number = getHighestY placements : placePiece placements' pieces' gas' (number - 1)
  where (res, gas') = dropPiece placements (newPoint 2 (getHighestY placements + 4)) piece gas
        placements' = foldl (flip insert) placements $ map (res +) piece

solve = do
  gas <- cycle <$> parseInput parseLine
  let floor = fromList $ map (`newPoint` 0) [0..6]
  let result = placePiece floor pieceList gas 2022
  printf "Highest y is %s\n" (show $ take 100 result)
