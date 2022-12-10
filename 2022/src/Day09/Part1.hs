module Day09.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (oneOf, endOfLine, endBy1, space)
import Helpers.Parsec (number, Parser, parseInput)
import Lib (Point, newPoint)
import qualified Data.Bifunctor
import Data.Set (Set, empty, insert, singleton, union, fromList, member)
import qualified Debug.Trace as Debug
import Data.List (intercalate)

type Move = (Point, Int)

parse :: Parser [(Char, Int)]
parse = ((,) <$> (oneOf "UDLR" <* space ) <*> number) `endBy1` endOfLine

dir :: Char -> Point
dir 'U' = newPoint 0 1
dir 'D' = newPoint 0 (-1)
dir 'L' = newPoint (-1) 0
dir 'R' = newPoint 1 0
dir _ = error "Invalid direction"

determineTailMove :: Point -> Point -> Point
determineTailMove h t
  | diff == sn = t
  | otherwise = t + sn
    where diff = negate (t - h)
          sn = signum diff


applyMove :: Point -> Point -> Set Point -> Move -> (Point, Point, Set Point)
applyMove h t visited (d, 0) = (h, t, visited)
applyMove h t visited (d, m) = applyMove h' t' visited' (d, m - 1)
  where h' = h + d
        t' = determineTailMove h' t
        visited' = insert t' visited

nextMove :: Point -> Point -> [Move] -> Set Point
nextMove h t [] = empty
nextMove h t (move:rest) = union v' $ nextMove h' t' rest
  where (h', t', v') = applyMove h t (singleton t) move

solve = do
  moves <- map (Data.Bifunctor.first dir) <$> parseInput parse
  let visited = nextMove (newPoint 0 0) (newPoint 0 0) moves
  -- let s = map (\y -> map (\x -> (if newPoint x y `member` visited then '#' else '.')) [0..5]) [0..5]
  --printf "%s\n" (intercalate "\n" (reverse s))
  printf "The number of visited tiles is %d" (length visited)
