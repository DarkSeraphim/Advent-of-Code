module Day15.Part2 (solve) where
import Text.Printf (printf)
import Text.Parsec (endOfLine, oneOf, many1, endBy1)
import Helpers.Parsec (Parser, parseInput)
import Data.Map (Map, insert, delete, notMember, (!), toList, fromList)
import Helpers.Point (Point, newPoint, getX, getY)
import Helpers.Input (toPointMapMaybe)
import Helpers.Output (showGrid)

instance Show Tile where
  show Robot = "@"
  show FoodL = "["
  show FoodR = "]"
  show Wall = "#"

grid :: Parser Grid
grid = toPointMapMaybe fromChar <$> many1 (oneOf "#.O@") `endBy1` endOfLine
  where fromChar '#' = Just Wall
        fromChar 'O' = Just FoodL
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

oneX :: Point
oneX = newPoint 1 0

theWidening :: Grid -> Grid
theWidening grid' = fromList (concatMap widen entries)
  where entries = toList grid'
        scale = newPoint 2 1
        widen (p, t) = if t /= Robot then [(p', t), (p' + oneX, t')] else [(p', t)]
          where p' = scale * p
                t' = if t == FoodL then FoodR else t

type Grid = Map Point Tile
data Tile = Robot | FoodL | FoodR | Wall deriving Eq
data Move = U | D | L | R

type Dir = Point


cascade :: Grid -> Point -> Dir -> Either Grid Grid
cascade grid' point dir
  | point `notMember` grid' = Right grid'
  | otherwise = case tile of
                  Robot -> cascade' grid' point
                  FoodL -> cascadeFood grid' point
                  FoodR -> cascadeFood grid' point
                  Wall -> Left grid'
  where tile = grid' ! point
        next = point + dir
        cascade' grid'' p = insert (p + dir) t' . delete p <$> cascade grid'' (p + dir) dir
          where t' = grid'' ! p
        cascadeFood :: Grid -> Point -> Either Grid Grid
        cascadeFood grid'' p
          | hor = cascade' grid' p
          | otherwise = mapLeft (const grid') (first >>= second)
          where first = cascade' grid'' p
                second g = cascade' g (getOther p tile)

        getOther :: Point -> Tile -> Point
        getOther p FoodL = p + oneX
        getOther p FoodR = p - oneX
        getOther _     _ = error "Not food"
        -- Horizontal means we don't do anything special
        -- Vertical requires special rollback for folds
        hor = getY dir == 0

mapLeft :: (Grid -> Grid) -> Either Grid Grid -> Either Grid Grid
mapLeft f (Left g) = Left $ f g
mapLeft _       r  = r

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
  where onlyFood = map fst $ filter ((== FoodL) . snd) (toList grid')
        scorePoint p = 100 * getY p + getX p

solve :: IO ()
solve = do
  (grid', moves) <- parseInput puzzleInput
  let result = foldl applyMove (theWidening grid') moves

  printf "Sum of all box GPS coordinates: %d\n" (scoreGrid result)
