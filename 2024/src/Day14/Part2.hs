{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
module Day14.Part2 (solve) where
import Text.Printf (printf)
import Helpers.Point (Point, newPoint, getX, getY)
import Helpers.Parsec (Parser, number, parseFile)
import Text.Parsec (string, char, endOfLine, endBy1)
import System.IO (stdin, hReady, hSetBuffering, hSetEcho, BufferMode (NoBuffering))
import Helpers.Output (clearScreen, showGrid)
import Debug.Trace (trace)
import Data.Map (fromList)

data Robot = Robot {pos :: Point, vel :: Point}

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

width :: Int
width = 101

height :: Int
height = 103

{--
-- Part of a faster solution I technically don't have knowledge about
moveUntilUnique :: [Robot] -> Int -> Int
moveUntilUnique robots' n
  | S.size (S.fromList positions) == length robots' = n
  | otherwise = moveUntilUnique robots' (n + 1)
  where moved = map (move n) robots'
        positions = map (clip (newPoint width height) . pos) moved

-- See above
solve' :: IO ()
solve' = do
  robots' <- parseInput robots
  let sol = moveUntilUnique robots' 1
  printf "Glitch in the matrix: %d\n" sol
--}

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' cs = do
          c <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (c:cs)

data Action = Up | Down | Left' | Right' | None

getAction :: IO Action
getAction = do
  key <- getKey

  trace (show key) $ return $ case key of
      "\ESC[A" -> Up
      "\ESC[B" -> Down
      "\ESC[D" -> Left'
      "\ESC[C" -> Right'
      _        -> None

loop :: [Robot] -> Int -> IO ()
loop robots' n = do
  clearScreen
  printf "At number %d\n\n" n
  printf "%s" (showGrid grid id '.')
  action <- getAction
  let n' = case action of
             Up -> n + 100
             Down -> n - 100
             Left' -> n - 1
             Right' -> n + 1
             None -> -1
  if n' < 0 then return () else loop robots' n'
  where moved = map (move n) robots'
        positions = map (clip (newPoint width height) . pos) moved
        grid = fromList $ map (, '#') positions

-- Spam the arrow keys to navigate time :D
solve :: IO ()
solve = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  robots' <- parseFile robots "inputs/14.txt"
  loop robots' 0

