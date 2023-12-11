module Day10.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGridMaybe)
import Helpers.Point (Point, newPoint)
import Data.Maybe (mapMaybe)
import Data.Map (Map, partition, keys, (!), insert, member)

data Pipe = START | NE | NS | NW | ES | EW | SW deriving (Eq, Show)

fromChar :: Char -> Maybe Pipe
fromChar '|' = Just NS
fromChar '-' = Just EW
fromChar 'F' = Just ES
fromChar 'L' = Just NE
fromChar '7' = Just SW
fromChar 'J' = Just NW
fromChar 'S' = Just START
fromChar _ = Nothing

findNextDeltas :: Pipe -> [Point]
findNextDeltas pipe = map (uncurry newPoint) (case pipe of
                                      NS -> [( 0, -1), ( 0, 1)]
                                      EW -> [( 1, 0), (-1,  0)]
                                      ES -> [( 1, 0), ( 0,  1)]
                                      NE -> [( 0, -1), ( 1,  0)]
                                      SW -> [( 0, 1), ( -1, 0)]
                                      NW -> [( 0, -1), (-1,  0)]
                                      _ -> error "This shouldn't happen"
                                   )

decideOnNext :: Map Point Pipe -> Point -> Point -> Maybe Point
decideOnNext grid cur prev
  | last options == prev = Just $ head options
  | head options == prev = Just $ last options
  | otherwise            = Nothing
  where options = map (+ cur) $ findNextDeltas (grid ! cur)

walkGrid :: Map Point Pipe -> Point -> Point -> Point -> Maybe Int
walkGrid grid start prev end
  | not $ member start grid = Nothing
  | start == end = Just 0
  | otherwise    = case next of
                     Just n -> (1 +) <$>  walkGrid grid n start end
                     Nothing -> Nothing
  where next = decideOnNext grid start prev

-- Previous being the current point shouldn't matter, as find will just grab the first match
findCycle :: Map Point Pipe -> Point -> Maybe Int
findCycle grid start = walkGrid grid next start start
  where next = (start +) $ head $ findNextDeltas (grid ! start)

allPipes :: [Pipe]
allPipes = [NE, NS, NW, ES, EW, SW]

solve :: IO ()
solve = do
  (start, grid) <- partition (== START) <$> parseGridMaybe fromChar
  let startPoint = head (keys start)
  let len = maximum $ mapMaybe  (\x -> findCycle (insert startPoint x grid) startPoint) allPipes
  printf "Max length cycle we found was %d\n" ((len + 1) `div` 2)
