module Helpers.Output (clearScreen, clearLine, showGrid, showGridWithKey) where
import Data.Map (Map, (!?), keys)
import Helpers.Point (Point, getX, newPoint, getY)
import Data.List (intercalate)
clearScreen :: IO ()
clearScreen = print "\033[2J"
clearLine :: IO ()
clearLine = print "\033[2K\r"


showGrid :: Map Point a -> (a -> Char) -> Char -> String
showGrid grid f = showGridWithKey grid (const f)

showGridWithKey :: Map Point a -> (Point -> a -> Char) -> Char -> String
showGridWithKey grid f def = intercalate "\n" s
      where s = map (\y -> map (\x -> let p = newPoint x y in maybe def (f p) (grid !? p)) [minX..maxX]) [minY..maxY]
            xs = map getX $ keys grid
            ys = map getY $ keys grid
            minX = minimum xs
            minY = minimum ys
            maxX = maximum xs
            maxY = maximum ys


