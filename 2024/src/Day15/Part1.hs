module Day15.Part1 (solve) where
import Text.Printf (printf)
import Text.Parsec (endOfLine, oneOf, many1, endBy1)
import Helpers.Parsec (Parser, parseInput)
import Data.Map (Map, insert, delete, notMember, (!), toList)
import Helpers.Point (Point, newPoint, getX, getY)
import Helpers.Input (toPointMapMaybe)

instance Show Tile where
  show Robot = "@"
  show Food = "O"
  show Wall = "#"

grid :: Parser Grid
grid = toPointMapMaybe fromChar <$> many1 (oneOf "#.O@") `endBy1` endOfLine
  where fromChar '#' = Just Wall
        fromChar 'O' = Just Food
        fromChar '@' = Just Robot
        fromChar  _  = Nothing


movements :: Parser [Move]
movements = concatMap (map fromChar) <$> many1 (oneOf "^v<>") `endBy1` endOfLine
  where fromChar '^' = U
        fromChar 'v' = D
        fromChar '<' = L
        fromChar '>' = R
        fromChar  _  = error "Unknown direction"

puzzleInput :: Parser (Grid, [Move])
puzzleInput = (,) <$> grid <* endOfLine <*> movements

type Grid = Map Point Tile
data Tile = Robot | Food | Wall deriving Eq
data Move = U | D | L | R

type Dir = Point

cascade :: Grid -> Point -> Dir -> Either Grid Grid
cascade grid' point dir
  | point `notMember` grid' = Right grid'
  | otherwise = case tile of
                  Robot -> i . d <$> cascade grid' next dir
                  Food -> i . d <$> cascade grid' next dir
                  Wall -> Left grid'
  where tile = grid' ! point
        next = point + dir
        i = insert next tile
        d = delete point

toDir :: Move -> Point
toDir U = newPoint   0 (-1)
toDir D = newPoint   0   1
toDir L = newPoint (-1)  0
toDir R = newPoint   1   0

applyMove :: Grid -> Move -> Grid
applyMove grid' move = extract result
  where (start, _) = head $ filter ((== Robot) . snd) $ toList grid'
        result = cascade grid' start (toDir move)
        extract (Left grid'') = grid''
        extract (Right grid'') = grid''

scoreGrid :: Grid -> Int
scoreGrid grid' = sum $ map scorePoint onlyFood
  where onlyFood = map fst $ filter ((== Food) . snd) (toList grid')
        scorePoint p = 100 * getY p + getX p

solve :: IO ()
solve = do
  (grid', moves) <- parseInput puzzleInput
  let result = foldl applyMove grid' moves

  printf "Sum of all box GPS coordinates: %d\n" (scoreGrid result)
