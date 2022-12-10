module Day09.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (oneOf, endOfLine, endBy1, space)
import Helpers.Parsec (number, Parser, parseInput)
import Lib (Point, newPoint)
import qualified Data.Bifunctor
import Data.Set (Set, empty, insert, singleton, union, fromList, member)
import qualified Debug.Trace as Debug
import Data.List (intercalate, reverse)

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

applyTailMove :: [Point] -> [Point]
applyTailMove [] = []
applyTailMove [h] = [h]
applyTailMove (h:t:tt) = h : applyTailMove (t':tt)
  where t' = determineTailMove h t



applyMove :: [Point] -> Move -> Set Point -> ([Point], Set Point)
applyMove [] _ _ = error "No" -- TODO: fix?
applyMove s (d, 0) v = (s, v)
applyMove (h:t) (d, m) v = applyMove t' (d, m - 1) v'
  where h' = h + d
        t' = applyTailMove (h' : t)
        v' = insert (last t') v


nextMove :: [Point] -> [Move] -> Set Point
nextMove h [] = empty
nextMove h (move:rest) = union v' $ nextMove h' rest
  where (h', v') = applyMove h move (singleton (last h))



solve = do
  moves <- map (Data.Bifunctor.first dir) <$> parseInput parse
  let t = map (const (newPoint 0 0)) [0..9]
  let visited = nextMove t moves
  -- let s = map (\y -> map (\x -> (if newPoint x y `member` visited then '#' else '.')) [0..5]) [0..5]
  --printf "%s\n" (intercalate "\n" (reverse s))
  printf "The number of visited tiles is %d" (length visited)
