{-# LANGUAGE NamedFieldPuns #-}
module Day14.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Point (Point, newPoint, getX, getY)
import Helpers.Parsec (Parser, number, parseInput)
import Text.Parsec (string, char, endOfLine, endBy1)
import Data.Map (fromListWith, elems)

-- p=0,4 v=3,-3

data Robot = Robot {pos :: Point, vel :: Point}
data Quadrant = NE | NW | SE | SW deriving (Eq, Ord)

robot :: Parser Robot
robot = Robot <$> (string "p=" *> point) <*> (string " v=" *> point)
  where point = newPoint <$> number <* char ',' <*> number :: Parser Point

robots :: Parser [Robot]
robots = robot `endBy1` endOfLine

move :: Int -> Robot -> Robot
move n (Robot {pos, vel}) = Robot {pos = pos + (scale * vel), vel}
  where scale = newPoint n n

clip :: Point -> Point -> Point
clip maxDim p = newPoint (getX p `mod'` maxX) (getY p `mod'` maxY)
  where maxX = getX maxDim
        maxY = getY maxDim
        x `mod'` y = if v < 0 then v + y else v
          where v = x `mod` y

pickQuadrant :: (Bool, Bool) -> Quadrant
pickQuadrant (False, False) = NW
pickQuadrant (True, False) = NE
pickQuadrant (False, True) = SW
pickQuadrant (True, True) = SE

width :: Int
width = 101

height :: Int
height = 103

quadrant :: Point -> [(Quadrant, [Point])]
quadrant p
  | getX p == xBound = []
  | getY p == yBound = []
  | otherwise = [(pickQuadrant bounds, [p])]
  where xBound = width `div` 2
        yBound = height `div` 2
        bounds = (getX p < xBound, getY p < yBound)

solve :: IO ()
solve = do
  robots' <- parseInput robots
  let moved = map (move 100) robots'
      positions = map (clip (newPoint width height) . pos) moved

  let quads = fromListWith (++) $ concatMap quadrant positions
  let factors = map length $ elems quads
  printf "Safety factor is %d\n" (product factors)
