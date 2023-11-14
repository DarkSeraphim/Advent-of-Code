module Day22.Part1 (solve) where
import Text.Printf (printf)
import Lib (Point, newPoint, getY, getX)
import Data.Maybe (catMaybes)
import Data.Map (fromList, keys, Map, (!), notMember)
import qualified Data.Map as M
import Data.List (sortOn)
import Helpers.Parsec (Parser, number, parseString)
import Text.Parsec (many1, char, (<|>), eof)
import Data.Functor (($>))
import Debug.Trace (trace)

data Cell = Empty | Wall deriving (Eq, Show)
data Rotation = L | R | N deriving (Eq, Show)

splitAtElem :: Eq a => a -> [a] -> ([a], [a])
splitAtElem t l = (left, right)
  where left = takeWhile (/= t) l
        right = drop 1 $ dropWhile (/= t) l

toCell :: Int -> Int -> Char -> Maybe (Point, Cell)
toCell y x '.' = Just (newPoint x y, Empty)
toCell y x '#' = Just (newPoint x y, Wall)
toCell _ _ _ = Nothing 

toRotation :: Parser Rotation
toRotation = (char 'R' $> R) <|> (char 'L' $> L)

toInstruction :: Parser (Int, Rotation)
toInstruction = (,) <$> number <*> (toRotation <|> (eof $> N))

toInstructions :: Parser [(Int, Rotation)]
toInstructions = many1 toInstruction 

rotate :: Point -> Rotation -> Point
rotate p R = newPoint (negate $ getY p) (getX p)
rotate p L = newPoint (getY p) (negate $ getX p)
rotate p N = p -- NOP

findOpposite :: Map Point Cell -> Point -> Point -> Point
findOpposite grid face point
  | point' `notMember` grid = point
  | otherwise = findOpposite grid face point'
  where point' = point - face

walk :: Map Point Cell -> Point -> Point -> Int -> Point
walk grid face point 0 = point
walk grid face point steps
  | grid ! point'' == Wall = point
  | otherwise = walk grid face point'' (steps - 1)
  where point' = point + face
        point'' = if point' `notMember` grid
                    then findOpposite grid face point'
                    else point'

process :: Map Point Cell -> Point -> Point -> [(Int, Rotation)] -> (Point, Point)
process grid face point [] = (point, face)
process grid face point ((steps, rot):rest) = process grid face' point' rest
  where face' = rotate face rot
        point' = walk grid face point steps

scoreFace :: Point -> Int
scoreFace p
  | x == 1    = 0
  | y == (-1) = 1
  | x == (-1) = 2
  | y == 1    = 4
  where x = getX p
        y = getY p

solve = do
  (gridLines, instr) <- splitAtElem "" . lines <$> getContents
  let grid = fromList $ catMaybes $ concat $ zipWith (\y gridLine -> zipWith (toCell y) [0..] gridLine) [0..] gridLines

  let openCells = M.filter (== Empty) grid
  
  let minY = minimum $ map getY $ keys grid

  let start = head $ sortOn getX $ keys $ M.filterWithKey (\k _ -> getY k == minY) openCells
  let face = newPoint 1 0
  instr' <- parseString toInstructions $ head instr
  printf "Starting at %s\n" (show start)
  printf "Instructions are %s\n" (show instr')

  let (pos, resFace) = process grid face start instr'
  let pos' = pos + newPoint 1 1

  let score = 1000 * (getY pos') + 4 * (getX pos') + (scoreFace resFace) 

  printf "Day 22? %s\n" (show score)
