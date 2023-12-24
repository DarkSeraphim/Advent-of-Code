module Day21.Part1 (solve) where
import Text.Printf (printf)
import Helpers.Parsec (parseGridMaybe)
import Helpers.Point (Point, neighbours)
import Data.Set (Set)
import Data.Map (Map, findWithDefault)
import qualified Data.Set as S
import qualified Data.Map as M

data Plot = Elf | Garden | Rocks deriving (Eq, Ord)

fromChar :: Char -> Maybe Plot
fromChar '#' = Just Rocks
fromChar '.' = Just Garden
fromChar 'S' = Just Elf
fromChar  _  = Nothing

findNext :: Map Point Plot -> Point -> [Point]
findNext grid start = fm neighbours
  where f = filter (\k -> Garden == findWithDefault Rocks k grid)
        m = map (start +)
        fm = f . m

flood :: Map Point Plot -> Set Point -> Int -> Set Point
flood    _ starts 0 = starts
flood grid starts n = flood grid starts' (n - 1)
  where starts' = S.fromList $ concatMap (findNext grid) (S.toList starts)

solve :: IO ()
solve = do
  grid <- parseGridMaybe fromChar
  let elf =  (fst . head . filter ((==Elf) . snd) . M.toList) grid
  let grid' = M.insert elf Garden grid
  let result = S.size (flood grid' (S.singleton elf) 64)
  printf "We can visit %d garden plots\n" result
